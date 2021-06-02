// Copyright (c) 2021, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

// Test throwing exceptions from an imported function.
import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:wasm/wasm.dart';

void main() {
  test('exception thrown from imported function', () {
    // void fn() {
    //   a();
    //   b();
    // }
    var data = Uint8List.fromList([
      0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x04, 0x01, 0x60, //
      0x00, 0x00, 0x02, 0x11, 0x02, 0x03, 0x65, 0x6e, 0x76, 0x01, 0x61, 0x00,
      0x00, 0x03, 0x65, 0x6e, 0x76, 0x01, 0x62, 0x00, 0x00, 0x03, 0x02, 0x01,
      0x00, 0x04, 0x05, 0x01, 0x70, 0x01, 0x01, 0x01, 0x05, 0x03, 0x01, 0x00,
      0x02, 0x06, 0x08, 0x01, 0x7f, 0x01, 0x41, 0x80, 0x88, 0x04, 0x0b, 0x07,
      0x0f, 0x02, 0x06, 0x6d, 0x65, 0x6d, 0x6f, 0x72, 0x79, 0x02, 0x00, 0x02,
      0x66, 0x6e, 0x00, 0x02, 0x0a, 0x10, 0x01, 0x0e, 0x00, 0x10, 0x80, 0x80,
      0x80, 0x80, 0x00, 0x10, 0x81, 0x80, 0x80, 0x80, 0x00, 0x0b,
    ]);

    var calledB = false;
    var thrownException = Exception('Hello exception!');
    var inst = WasmModule(data).instantiate().addFunction('env', 'a', () {
      throw thrownException;
    }).addFunction('env', 'b', () {
      calledB = true;
    }).build();
    var fn = inst.lookupFunction('fn');
    expect(() => fn(), throwsA(thrownException));
    expect(calledB, isFalse);

    var calledA = false;
    inst = WasmModule(data).instantiate().addFunction('env', 'a', () {
      calledA = true;
    }).addFunction('env', 'b', () {
      calledB = true;
    }).build();
    fn = inst.lookupFunction('fn');
    fn();
    expect(calledA, isTrue);
    expect(calledB, isTrue);
  });
}
