// Copyright (c) 2017, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.
// @dart=2.9
class A {
  A();
  factory A.fisk() = B;
}

class B extends A {}

main() {
  new A.fisk();
}
