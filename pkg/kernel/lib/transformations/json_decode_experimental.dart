// Copyright (c) 2019, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

library kernel.transformations.json_decode_experimental;

import '../core_types.dart';
import '../ast.dart';

final _jsonTransformerUri =
    Uri(scheme: 'package', path: 'json_transformer/json_transformer.dart');

class JsonDecodeExperimentalTransformer extends Transformer {
  final CoreTypes coreTypes;
  final InterfaceType iterableDynamic;
  final InterfaceType mapStringDynamic;
  final Class mapEntryClass;

  JsonDecodeExperimentalTransformer(this.coreTypes)
      : iterableDynamic =
            InterfaceType(coreTypes.iterableClass, const [DynamicType()]),
        mapStringDynamic = InterfaceType(coreTypes.mapClass,
            [coreTypes.stringClass.thisType, DynamicType()]),
        mapEntryClass = coreTypes.index.getClass('dart:core', 'MapEntry');

  @override
  Expression visitStaticInvocation(StaticInvocation node) {
    node.transformChildren(this);
    final procedure = node.target;
    if (!procedure.isStatic) return node;
    if (procedure.enclosingLibrary.importUri == _jsonTransformerUri &&
        procedure.name.name == 'jsonDecodeExperimental') {
      // We know we have exactly one type argument.
      var typeArg = node.arguments.types.first as InterfaceType;
      var jsonVar = VariableDeclaration(
        'json',
        type: const DynamicType(),
        initializer: StaticInvocation(
            coreTypes.index
                .getLibrary('dart:convert')
                .procedures
                .firstWhere((p) => p.name.name == 'jsonDecode'),
            Arguments(node.arguments.positional)),
      );
      var expr = MethodInvocation(
          FunctionExpression(FunctionNode(
            Block([
              jsonVar,
              ReturnStatement(_newInstance(typeArg, VariableGet(jsonVar))),
            ]),
          )),
          Name('call'),
          Arguments.empty());
      return expr;
    } else {
      return node;
    }
  }

  Expression _newInstance(DartType type, Expression argExpr) {
    if (type is DynamicType) {
      return argExpr;
    } else if (type is InterfaceType) {
      var library = type.classNode.enclosingLibrary;
      if (library.importUri.scheme != 'dart') {
        if (library.importUri == _jsonTransformerUri) {
          switch (type.classNode.name) {
            case 'LazyList':
            case 'LazyMap':
              return _newLazyCollection(type, argExpr);
            default:
              throw UnsupportedError('Unsupported type $type');
          }
        }
        return _newCustomInstance(type, argExpr);
      } else if (library == coreTypes.coreLibrary) {
        return _newCoreInstance(type, argExpr);
      }
    }

    throw '''
Unsupported type: ${type}
''';
  }

  /// Creates a new instance of the core type [type] from the object referenced
  /// by [argExpr].
  Expression _newCoreInstance(InterfaceType type, Expression argExpr) {
    switch (type.className.canonicalName.name) {
      case 'Null':
        return argExpr;
      case 'String':
      case 'bool':
      case 'int':
      case 'double':
        return AsExpression(argExpr, type);
      case 'Iterable':
        var valueType = type.typeArguments.first;
        var vParam = VariableDeclaration('v', type: const DynamicType());
        return MethodInvocation(
          AsExpression(argExpr, iterableDynamic),
          Name('map'),
          Arguments([
            FunctionExpression(
              FunctionNode(
                ReturnStatement(
                  _newInstance(valueType, VariableGet(vParam)),
                ),
                returnType: valueType,
                positionalParameters: [vParam],
              ),
            ),
          ], types: [
            valueType,
          ]),
        );
      case 'List':
        var valueType = type.typeArguments.first;
        var vParam = VariableDeclaration('v', type: const DynamicType());
        return MethodInvocation(
            MethodInvocation(
              AsExpression(argExpr, iterableDynamic),
              Name('map'),
              Arguments([
                FunctionExpression(
                  FunctionNode(
                    ReturnStatement(
                      _newInstance(valueType, VariableGet(vParam)),
                    ),
                    returnType: valueType,
                    positionalParameters: [vParam],
                  ),
                ),
              ], types: [
                valueType,
              ]),
            ),
            Name('toList'),
            Arguments.empty());
      case 'Map':
        var keyType = type.typeArguments.first;
        var valueType = type.typeArguments[1];
        var kParam = VariableDeclaration('k', type: const DynamicType());
        var vParam = VariableDeclaration('v', type: const DynamicType());
        return MethodInvocation(
            AsExpression(argExpr, mapStringDynamic),
            Name('map'),
            Arguments([
              FunctionExpression(
                FunctionNode(
                  ReturnStatement(ConstructorInvocation(
                      mapEntryClass.constructors.single,
                      Arguments([
                        _newInstance(keyType, VariableGet(kParam)),
                        _newInstance(valueType, VariableGet(vParam))
                      ], types: [
                        keyType,
                        valueType,
                      ]))),
                  returnType:
                      InterfaceType(mapEntryClass, [keyType, valueType]),
                  positionalParameters: [kParam, vParam],
                ),
              ),
            ], types: [
              keyType,
              valueType,
            ]));
      default:
        throw '''
Unsupported core type: ${type.className};
  ''';
    }
  }

  /// Creates a custom instance of [type] from the object referenced by [argExpr].
  ConstructorInvocation _newCustomInstance(
      InterfaceType type, Expression argExpr) {
    var clazz = type.classNode;

    var constructor = clazz.constructors
        .firstWhere((c) => c.name.name == '', orElse: () => null);
    if (constructor == null) {
      throw UnsupportedError('''
Unable to find an unnamed constructor for type:

  class: ${clazz.name}
  library: ${clazz.enclosingLibrary.importUri}

jsonAutoDecode only works for core types and types with unnamed constructors.
''');
    }

    var positionalParams = constructor.function.positionalParameters;
    var positionalArgs = [
      for (var param in positionalParams) _parameterValue(type, param, argExpr)
    ];

    var namedParams = constructor.function.namedParameters;
    var namedArgs = [
      for (var param in namedParams)
        NamedExpression(param.name, _parameterValue(type, param, argExpr))
    ];

    return ConstructorInvocation(
        constructor,
        Arguments(
          positionalArgs,
          named: namedArgs,
          types: type.typeArguments,
        ));
  }

  Expression _parameterValue(InterfaceType parent,
      VariableDeclaration methodParam, Expression argExpr) {
    // First, build the expression to get the value out of the map
    Expression mapValueExpr;

    if (argExpr is VariableGet || argExpr is MethodInvocation) {
      mapValueExpr = MethodInvocation(
        AsExpression(argExpr, mapStringDynamic),
        Name('[]'),
        Arguments([StringLiteral(methodParam.name)]),
      );
    } else {
      throw '''
  Unrecognized argument type:

    runtimeType: ${argExpr.runtimeType}
    value: $argExpr
  ''';
    }

    // Now build the actual argument expression based on the type of the argument.
    if (methodParam.type is! InterfaceType) {
      throw '''
  Unsupported type, only classes are supported: ${methodParam.type}
  ''';
    }
    var paramType = methodParam.type as InterfaceType;

    var newTypeArgs = paramType.typeArguments.map((typeArg) {
      if (typeArg is TypeParameterType) {
        var index = parent.classNode.typeParameters.indexOf(typeArg.parameter);
        return parent.typeArguments[index];
      }
      return typeArg;
    }).toList();

    paramType = InterfaceType(paramType.classNode, newTypeArgs);
    return _newInstance(paramType, mapValueExpr);
  }

  ConstructorInvocation _newLazyCollection(
      InterfaceType type, Expression argExpr) {
    var clazz = type.classNode;
    var targetType = type.typeArguments.first;
    var vParam = VariableDeclaration('v', type: const DynamicType());
    var converter = FunctionExpression(
      FunctionNode(
          ReturnStatement(_newInstance(targetType, VariableGet(vParam))),
          returnType: targetType,
          positionalParameters: [vParam]),
    );
    return ConstructorInvocation(
        clazz.constructors.firstWhere((c) => c.name.name == ''),
        Arguments(
          [argExpr, converter],
          types: type.typeArguments,
        ));
  }
}
