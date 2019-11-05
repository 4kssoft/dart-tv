// Copyright (c) 2019, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

library kernel.transformations.function_apply;

import '../ast.dart';

class FunctionApplyTransformer extends Transformer {
  @override
  StaticInvocation visitStaticInvocation(StaticInvocation node) {
    node.transformChildren(this);
    final procedure = node.target;
    if (!procedure.isStatic) return node;
    final clazz = procedure.enclosingClass;
    if (clazz == null) return node;
    final clazzName = clazz?.canonicalName;
    if (clazzName == null) return node;
    if (clazzName.parent.name == 'dart:core' &&
        clazzName.name == 'Function' &&
        procedure.name.name == 'apply') {
      var args = node.arguments.positional;
      // The function we want to invoke.
      var fnExpr = args.first;
      if (fnExpr is ConstantExpression) {
        final targetFn = fnExpr.constant;
        if (targetFn is TearOffConstant) {
          List<Expression> positionalArgs;
          final providedPositionalArgs = args[1];
          if (providedPositionalArgs is NullLiteral) {
            positionalArgs = [];
          } else if (providedPositionalArgs is ListLiteral) {
            positionalArgs = providedPositionalArgs.expressions;
          } else if (providedPositionalArgs is ListConstant) {
            positionalArgs = (providedPositionalArgs as ListConstant)
                .entries
                .map((c) => c.asExpression());
          } else {
            print(
                'didnt understand list type $providedPositionalArgs ${providedPositionalArgs.runtimeType}');
            return node;
          }
          var namedArgs = <NamedExpression>[];
          if (args.length == 3) {
            var namedArgsMap = args[2];
            var namedParams = targetFn.procedure.function.namedParameters;
            if (namedArgsMap is MapLiteral) {
              for (var param in namedParams) {
                namedArgs.add(NamedExpression(
                    param.name,
                    namedArgsMap.entries
                        .firstWhere((e) =>
                            ((e.key as ConstantExpression).constant
                                    as SymbolConstant)
                                .name ==
                            param.name)
                        .value));
              }
            } else if (namedArgsMap is VariableGet) {
              for (var param in namedParams) {
                namedArgs.add(NamedExpression(
                    param.name,
                    MethodInvocation(namedArgsMap, Name('[]'),
                        Arguments([SymbolLiteral(param.name)]))));
              }
            } else {
              print(namedArgsMap);
              print(namedArgsMap.runtimeType);
              return node;
            }
          }
          return StaticInvocation(targetFn.procedureReference.asProcedure,
              Arguments(positionalArgs, named: namedArgs));
        }
      }
    }
    return node;
  }
}
