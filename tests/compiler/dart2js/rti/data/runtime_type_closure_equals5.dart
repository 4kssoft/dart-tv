// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

// @dart = 2.7

import 'package:expect/expect.dart';

/*prod:nnbd-off.class: Class1:needsArgs*/
/*spec:nnbd-off.class: Class1:direct,explicit=[Class1.T],needsArgs*/
class Class1<T> {
  /*spec:nnbd-off.member: Class1.:*/
  /*prod:nnbd-off.member: Class1.:*/
  Class1();

  /*spec:nnbd-off.member: Class1.method1a:needsSignature*/
  /*prod:nnbd-off.member: Class1.method1a:needsSignature*/
  T method1a() => null;

  /*spec:nnbd-off.member: Class1.method1b:needsSignature*/
  /*prod:nnbd-off.member: Class1.method1b:needsSignature*/
  T method1b() => null;

  /*spec:nnbd-off.member: Class1.method2:needsSignature*/
  /*prod:nnbd-off.member: Class1.method2:needsSignature*/
  T method2(T t, String s) => t;
}

/*spec:nnbd-off.class: Class2:*/
/*prod:nnbd-off.class: Class2:*/
class Class2<T> {
  /*spec:nnbd-off.member: Class2.:*/
  /*prod:nnbd-off.member: Class2.:*/
  Class2();
}

/*spec:nnbd-off.member: main:*/
/*prod:nnbd-off.member: main:*/
main() {
  var c = new Class1<int>();

  Expect.isTrue(c.method1a.runtimeType == c.method1b.runtimeType);
  Expect.isFalse(c.method1a.runtimeType == c.method2.runtimeType);
  new Class2<int>();
}
