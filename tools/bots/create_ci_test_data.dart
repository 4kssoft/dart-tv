// Copyright (c) 2020, the Dart project authors. Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE.md file.

/// This scripts is a very simple test runner that produces a results.json
/// from a hard-coded list and can be used to create test data for the test
/// result infrastructure on a builder.

import 'dart:io';
import 'dart:convert';

import 'package:args/args.dart';

const suite = "ci-test-data";

const results = [
  Result("test_a", "CompileTimeError"),
  Result("test_b", "RuntimeError"),
];

class Result {
  final String name;
  final String result;

  const Result(this.name, this.result);

  Map<String, Object> toJson(String configuration) => {
        "test_name": name,
        "result": result,
        "configuration": configuration,
        "suite": suite,
        "time_ms": 0,
        "expected": "Pass",
        "matches": result == "Pass",
        "name": "$suite/$name",
      };
}

main(args) {
  var parser = new ArgParser()
    ..addOption("named-configuration",
        abbr: "n", help: "configuration name emitted json results")
    ..addOption("output-directory",
        help: "directory to which results.json is written");
  var parsedArguments = parser.parse(args);
  var configuration = parsedArguments["named-configuration"];
  var outputDirectory = parsedArguments["output-directory"];
  if (configuration == null || outputDirectory == null) {
    print("usage: create_ci_test_data -n<CONFIGURATION> "
        "--output-directory <PATH>");
    exit(1);
  }
  var extendedResults =
      results.map((result) => jsonEncode(result.toJson(configuration)));
  StringBuffer sb = StringBuffer();
  for (var result in extendedResults) {
    sb.writeln(result);
  }
  var outputFile =
      File.fromUri(Uri.directory(outputDirectory).resolve("results.json"));
  outputFile.writeAsStringSync(sb.toString());
}
