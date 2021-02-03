// Copyright (c) 2015, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

// @dart = 2.7

import 'package:expect/expect.dart';
import 'package:async_helper/async_helper.dart';
import 'package:compiler/src/compiler.dart';
import 'package:compiler/src/commandline_options.dart';
import 'package:compiler/src/elements/entities.dart';
import 'package:compiler/src/inferrer/abstract_value_domain.dart';
import 'package:compiler/src/inferrer/types.dart';
import 'package:compiler/src/world.dart' show JClosedWorld;
import '../inference/type_mask_test_helper.dart';
import '../helpers/memory_compiler.dart';

const Map<String, String> MEMORY_SOURCE_FILES = const {
  'main.dart': r"""
import 'package:expect/expect.dart';

int method(String arg) => arg.length;

@pragma('dart2js:assumeDynamic')
int methodAssumeDynamic(String arg) => arg.length;

@pragma('dart2js:noInline')
int methodNoInline(String arg) => arg.length;

void main(List<String> args) {
  print(method(args[0]));
  print(methodAssumeDynamic('foo'));
  print(methodNoInline('bar'));
}
"""
};

main() {
  asyncTest(() async {
    await runTest();
  });
}

runTest() async {
  CompilationResult result = await runCompiler(
      memorySourceFiles: MEMORY_SOURCE_FILES, options: [Flags.testMode]);
  Compiler compiler = result.compiler;
  JClosedWorld closedWorld = compiler.backendClosedWorldForTesting;
  AbstractValueDomain commonMasks = closedWorld.abstractValueDomain;
  Expect.isFalse(compiler.compilationFailed, 'Unsuccessful compilation');

  void testTypeMatch(
      FunctionEntity function,
      AbstractValue expectedParameterType,
      AbstractValue expectedReturnType,
      GlobalTypeInferenceResults results) {
    closedWorld.elementEnvironment.forEachParameterAsLocal(
        closedWorld.globalLocalsMap, function, (Local parameter) {
      AbstractValue type = results.resultOfParameter(parameter);
      Expect.equals(
          expectedParameterType, simplify(type, commonMasks), "$parameter");
    });
    if (expectedReturnType != null) {
      AbstractValue type = results.resultOfMember(function).returnType;
      Expect.equals(
          expectedReturnType, simplify(type, commonMasks), "$function");
    }
  }

  void test(String name,
      {bool expectNoInline: false,
      AbstractValue expectedParameterType: null,
      AbstractValue expectedReturnType: null,
      bool expectAssumeDynamic: false}) {
    LibraryEntity mainApp = closedWorld.elementEnvironment.mainLibrary;
    FunctionEntity method =
        closedWorld.elementEnvironment.lookupLibraryMember(mainApp, name);
    Expect.isNotNull(method);
    Expect.equals(
        expectNoInline,
        closedWorld.annotationsData.hasNoInline(method),
        "Unexpected annotation of @pragma('dart2js:noInline') on '$method'.");
    Expect.equals(
        expectAssumeDynamic,
        closedWorld.annotationsData.hasAssumeDynamic(method),
        "Unexpected annotation of @pragma('dart2js:assumeDynamic') on "
        "'$method'.");
    GlobalTypeInferenceResults results =
        compiler.globalInference.resultsForTesting;
    if (expectAssumeDynamic) {
      testTypeMatch(
          method, closedWorld.abstractValueDomain.dynamicType, null, results);
    }
  }

  test('method');
  test('methodAssumeDynamic', expectAssumeDynamic: true);
  test('methodNoInline', expectNoInline: true);
}
