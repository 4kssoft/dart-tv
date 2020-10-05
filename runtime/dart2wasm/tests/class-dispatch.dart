// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.
//
// More complex class translation example. Requires global dispatch
// table calls.
class Animal {
    Animal(int x, int y) {
        x_ = x;
        y_ = y;
    }
    @pragma("vm:never-inline")
    int bark(int z) {
      return 1998;  // Shouldn't be reached.
    }

    int x_;
    int y_;
}

class Dog extends Animal {
    Dog(int t) : super(t, t){
        t_ = t;
    }
    int t_;

    @pragma("vm:never-inline")
    @override
    int bark(int z) {
        return t_ + 2 * z;
    }
}

class Cat extends Animal {
    Cat(int t): super(t, t) {
        t_ = t;
    }

    @pragma("vm:never-inline")
    @override
    int bark(int z) {
        return t_ + z;
    }

    int t_;
}

@pragma("vm:never-inline")
int dispatch(Animal x) {
   return x.bark(5);
}

@pragma("vm:never-inline")
void main() {
    Cat cat = new Cat(3);
    Dog dog = new Dog(5);
    print(dispatch(cat) + dispatch(dog));  // (3 + 5) + (5 + 2 * 5) =
                                           // 8 + 15 = 23
}
