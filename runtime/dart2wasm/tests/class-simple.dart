// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.
//
// Simple class translation example. Notably, doesn't require non-static
// instance calls.
class Animal {
    Animal(int x, int y) {
        x_ = x;
        y_ = y;
    }
    int x_;
    int y_;
}

class Dog extends Animal {
    Dog(int t) : super(t, t + 1) {
        t_ = t;
    }
    int bark(int z) {
        print(x_ + y_ + z);
    }
    int t_;
}

@pragma("vm:never-inline")
Animal f() {
    Dog t = Dog(13);
    t.bark(100); // 100 + 13 + 14 = 127
    return t;
}

void main() {
    Animal t = f();
    print(t.x_); // 13
    print(t.y_); // 14
}
