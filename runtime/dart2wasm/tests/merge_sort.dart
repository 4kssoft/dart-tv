// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

// Merge-sort benchmark.
// Increase size and remove print when doing timing.

void main() {
  final int size = 100;
  final int min = 1;
  final int max = 1000;
  Link list = makeRandomList(size, min, max);
  Link sorted = sortList(list, size);
  printList(sorted, size);
}

Link makeRandomList(int size, int min, int max) {
  Random r = Random(20201001);
  Link list = null;
  for (int i = 0; i < size; i++) {
    list = Link(r.next(min, max), list);
  }
  return list;
}

void printList(Link list, int size) {
  for (int i = 0; i < size; i++) {
    print(list.value);
    list = list.tail;
  }
}

Link sortList(Link list, int size) {
  if (size <= 1) return list;
  return _subSort(list, size).first;
}

Pair _subSort(Link list, int size) {
  if (size == 1) {
    Pair p = Pair(list, list.tail);
    list.tail = null;
    return p;
  }
  int half = size ~/ 2;
  Pair s1 = _subSort(list, half);
  Pair s2 = _subSort(s1.second, size - half);
  Link merged = _merge(s1.first, half, s2.first, size - half);
  return Pair(merged, s2.second);
}

Link _merge(Link list1, int size1, Link list2, int size2) {
  Link head = Link.empty();
  Link last = head;
  while (size1 > 0 && size2 > 0) {
    if (list1.value <= list2.value) {
      last.tail = list1;
      list1 = list1.tail;
      size1 -= 1;
    } else {
      last.tail = list2;
      list2 = list2.tail;
      size2 -= 1;
    }
    last = last.tail;
  }
  if (size1 == 0) {
    last.tail = list2;
  } else {
    last.tail = list1;
  }
  return head.tail;
}

class Link {
  int value;
  Link tail;

  Link(this.value, this.tail);

  Link.empty()
      : value = 0,
        tail = null;
}

class Pair {
  Link first;
  Link second;

  Pair(this.first, this.second);
}

class Random {
  int seed;

  Random(this.seed);

  int next(int min, int max) {
    seed = (seed * 987654321) ^ (seed ~/ 43);
    return min + (seed & 0xFFFFFFFFFFFFF) % (max - min + 1);
  }
}
