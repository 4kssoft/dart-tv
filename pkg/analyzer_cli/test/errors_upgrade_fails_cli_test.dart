// Copyright (c) 2017, the Dart project authors. Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'dart:io';

import 'package:analyzer_cli/src/driver.dart';
import 'package:analyzer_cli/src/options.dart';
import 'package:path/path.dart' as path;
import 'package:test/test.dart';
import 'package:test_reflective_loader/test_reflective_loader.dart';

import 'utils.dart';

void main() {
  defineReflectiveTests(ErrorUpgradeFailsCli);
}

@reflectiveTest
class ErrorUpgradeFailsCli {
  late StringSink savedOutSink, savedErrorSink;
  late int savedExitCode;
  late ExitHandler savedExitHandler;

  void setUp() {
    savedOutSink = outSink;
    savedErrorSink = errorSink;
    savedExitHandler = exitHandler;
    savedExitCode = exitCode;
    exitHandler = (code) => exitCode = code;
    outSink = StringBuffer();
    errorSink = StringBuffer();
  }

  void tearDown() {
    outSink = savedOutSink;
    errorSink = savedErrorSink;
    exitCode = savedExitCode;
    exitHandler = savedExitHandler;
  }

  Future<void> test_once() async {
    var testDir = path.join(testDirectory, 'data', 'error_upgrade_fails_cli');
    var driver = Driver();
    await driver.start([path.join(testDir, 'foo.dart')]);

    expect(exitCode, 3);
  }
}
