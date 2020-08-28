// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

const String valueClass = "valueClass";

@valueClass
class Animal {
  final int numLegs;
}

@valueClass
class Cat implements Animal {
  final int numLegs;
  final int numWhiskers;
}

main() {
  Cat firstCat = Cat(numLegs: 4, numWhiskers: 10);
  Cat secondCat = Cat(numLegs: 4, numWhiskers: 10);
  Cat thirdCat = Cat(numLegs: 4, numWhiskers: 0);

  expect(true, firstCat == secondCat);
  expect(false, firstCat == thirdCat);

  expect(true, firstCat.hashCode == secondCat.hashCode);
  expect(false, firstCat.hashCode == thirdCat.hashCode);
}

expect(expected, actual, [expectNull = false]) {
  if (expectNull) {
    expected = null;
  }
  if (expected != actual) {
    throw 'Mismatch: expected=$expected, actual=$actual';
  }
}