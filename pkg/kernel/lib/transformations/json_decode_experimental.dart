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

  JsonDecodeExperimentalTransformer(this.coreTypes);

  @override
  Expression visitStaticInvocation(StaticInvocation node) {
    node.transformChildren(this);
    final procedure = node.target;
    if (!procedure.isStatic) return node;
    if (procedure.enclosingLibrary.importUri == _jsonTransformerUri &&
        procedure.name.name == 'jsonDecodeExperimental') {
      // We know we have exactly one type argument.
      var typeArg = node.arguments.types.first as InterfaceType;
      var expr = _newInstance(typeArg, node.arguments.positional.first);
      print(expr.leakingDebugToString());
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
              var index =
                  type.classNode.typeParameters.indexOf(typeArg.parameter);
              return type.typeArguments[index];
            }
            return typeArg;
          }).toList();

          paramType = InterfaceType(paramType.classNode, newTypeArgs);

          namedArgs.add(NamedExpression(
              param.name, _newInstance(paramType, mapValueExpr)));
        }

        return ConstructorInvocation(
            defaultConstructor,
            Arguments(
              [] /* TODO: support positional args */,
              named: namedArgs,
              types: type.typeArguments,
            ));
      } else if (library == coreTypes.coreLibrary) {
        switch (type.className.canonicalName.name) {
          case 'Null':
            return argExpr;
          case 'String':
          case 'bool':
          case 'int':
          case 'double':
            return AsExpression(argExpr, type);
          case 'List':
            var valueType = type.typeArguments.first;
            var vParam = VariableDeclaration('v', type: const DynamicType());
            return MethodInvocation(
                MethodInvocation(
                  argExpr,
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
            var keyType = type.typeArguments.first as InterfaceType;
            if (keyType.classNode != coreTypes.stringClass) {
              throw '''
Only `String` is allowed as a map key type, but got $keyType.
''';
            }
            var valueType = type.typeArguments[1] as InterfaceType;
            var kParam = VariableDeclaration('k', type: keyType);
            var vParam = VariableDeclaration('v', type: const DynamicType());
            var mapEntryClass =
                coreTypes.index.getClass('dart:core', 'MapEntry');
            return MethodInvocation(
                argExpr,
                Name('map'),
                Arguments([
                  FunctionExpression(
                    FunctionNode(
                      ReturnStatement(ConstructorInvocation(
                          mapEntryClass.constructors.single,
                          Arguments([
                            VariableGet(kParam),
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
    }

    throw '''
Unsupported type: ${type}
''';
  }
}
