// Copyright (c) 2021, the Dart project authors. Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'package:analysis_server/src/services/correction/dart/abstract_producer.dart';
import 'package:analysis_server/src/services/correction/fix.dart';
import 'package:analyzer/dart/ast/ast.dart';
import 'package:analyzer_plugin/utilities/change_builder/change_builder_core.dart';
import 'package:analyzer_plugin/utilities/fixes/fixes.dart';
import 'package:analyzer_plugin/utilities/range_factory.dart';

class ReplaceFinalWithVar extends CorrectionProducer {
  @override
  FixKind get fixKind => DartFixKind.REPLACE_FINAL_WITH_VAR;

  @override
  FixKind get multiFixKind => DartFixKind.REPLACE_FINAL_WITH_VAR_MULTI;

  @override
  Future<void> compute(ChangeBuilder builder) async {
    var target = node;
    if (target is VariableDeclarationList) {
      if (target.type == null) {
        await builder.addDartFileEdit(file, (builder) {
          builder.addSimpleReplacement(range.token(target.keyword), 'var');
        });
      }
    }
  }

  /// Return an instance of this class. Used as a tear-off in `FixProcessor`.
  static ReplaceFinalWithVar newInstance() => ReplaceFinalWithVar();
}
