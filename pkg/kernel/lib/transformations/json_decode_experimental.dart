// Copyright (c) 2019, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

library kernel.transformations.json_decode_experimental;

import '../ast.dart';

final _jsonTransformerUri =
    Uri(scheme: 'package', path: 'json_transformer/json_transformer.dart');

final _dartCore = Uri(scheme: 'dart', path: 'core');

InvocationExpression _newInstance(Class clazz, Expression mapExpr) {
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

    if (mapExpr is MapLiteral) {
      mapValueExpr = mapExpr.entries
          .firstWhere((e) => (e.key as StringLiteral).value == param.name)
          .value;
    } else if (mapExpr is VariableGet || mapExpr is MethodInvocation) {
      mapValueExpr = MethodInvocation(
          mapExpr, Name('[]'), Arguments([StringLiteral(param.name)]));
    } else {
      throw '''
Unrecognized type of map argument:
runtimeType: ${mapExpr.runtimeType}
value: $mapExpr
''';
    }

    // Now build the actual argument expression based on the type of the argument.
    Expression argExpr;

    var type = param.type;
    if (type is InterfaceType) {
      var library = type.classNode.enclosingLibrary;
      if (library.importUri.scheme != 'dart') {
        argExpr = _newInstance(type.classNode, mapValueExpr);
      } else if (library.importUri == _dartCore) {
        switch (type.className.canonicalName.name) {
          case 'String':
          case 'bool':
          case 'int':
          case 'double':
            argExpr = AsExpression(mapValueExpr, type);
            break;
          case 'List':
            var generic = type.typeArguments.first;
            print('''
first: $generic
all: ${type.typeArguments}     
''');
            argExpr = AsExpression(mapValueExpr, type);
            break;
          case 'Map':
            var generic = type.typeArguments.first;
            print('''
first: $generic
all: ${type.typeArguments}     
''');
            argExpr = AsExpression(mapValueExpr, type);
            break;
          default:
            throw '''
Unsupported core type: ${type.className};
  ''';
        }
      } else {
        throw '''
Unsupported type: ${type.className}
''';
      }
    } else {
      throw '''
Unsupported type, only classes are supported: ${type}
''';
    }

    namedArgs.add(NamedExpression(param.name, argExpr));
  }

  return ConstructorInvocation(defaultConstructor,
      Arguments([] /* TODO: support positional args */, named: namedArgs));
}

class JsonDecodeExperimentalTransformer extends Transformer {
  @override
  InvocationExpression visitStaticInvocation(StaticInvocation node) {
    node.transformChildren(this);
    final procedure = node.target;
    if (!procedure.isStatic) return node;
    if (procedure.enclosingLibrary.importUri == _jsonTransformerUri &&
        procedure.name.name == 'jsonDecodeExperimental') {
      // We know we have exactly one type argument.
      var typeArg = node.arguments.types.first as InterfaceType;
      var expr =
          _newInstance(typeArg.classNode, node.arguments.positional.first);
      return expr;
    } else {
      return node;
    }
  }
}
