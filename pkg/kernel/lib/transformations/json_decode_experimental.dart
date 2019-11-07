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
  final InterfaceType mapDynamic;
  final Class mapEntryClass;

  JsonDecodeExperimentalTransformer(this.coreTypes)
      : iterableDynamic =
            InterfaceType(coreTypes.iterableClass, const [DynamicType()]),
        mapDynamic = InterfaceType(
            coreTypes.mapClass, const [DynamicType(), DynamicType()]),
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
            AsExpression(argExpr, mapDynamic),
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

    var defaultConstructor = clazz.constructors
        .firstWhere((c) => c.name.name == '', orElse: () => null);
    if (defaultConstructor == null) {
      throw 'no unnamed constructor for class: ${clazz.name} from library: ${clazz.enclosingLibrary.importUri}';
    }

    // TODO: positional parameters
    // var positionalArgs = <Expression>[];
    // var positionalParams = defaultConstructor.function.positionalParameters;

    var namedArgs = <NamedExpression>[];
    var namedParams = defaultConstructor.function.namedParameters;

    for (var param in namedParams) {
      // First, build the expression to get the value out of the map
      Expression mapValueExpr;

      if (argExpr is MapLiteral) {
        mapValueExpr = argExpr.entries
            .firstWhere((e) => (e.key as StringLiteral).value == param.name)
            .value;
      } else if (argExpr is VariableGet || argExpr is MethodInvocation) {
        mapValueExpr = MethodInvocation(
            argExpr, Name('[]'), Arguments([StringLiteral(param.name)]));
      } else {
        throw '''
  Unrecognized type of map argument:
  runtimeType: ${argExpr.runtimeType}
  value: $argExpr
  ''';
      }

      // Now build the actual argument expression based on the type of the argument.
      if (param.type is! InterfaceType) {
        throw '''
  Unsupported type, only classes are supported: ${type}
  ''';
      }
      var paramType = param.type as InterfaceType;

      var newTypeArgs = paramType.typeArguments.map((typeArg) {
        if (typeArg is TypeParameterType) {
          var index = type.classNode.typeParameters.indexOf(typeArg.parameter);
          return type.typeArguments[index];
        }
        return typeArg;
      }).toList();

      paramType = InterfaceType(paramType.classNode, newTypeArgs);

      namedArgs.add(
          NamedExpression(param.name, _newInstance(paramType, mapValueExpr)));
    }

    return ConstructorInvocation(
        defaultConstructor,
        Arguments(
          [] /* TODO: support positional args */,
          named: namedArgs,
          types: type.typeArguments,
        ));
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
