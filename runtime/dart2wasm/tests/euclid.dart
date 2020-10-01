// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.
//
// Recursive Euclid's Algorithm.
//
// Expected output:
// 1
// 4
//
int gcd(int a, int b) {
    if (b == 0) {
        return a;
    }
    return gcd(b, a % b);
}

void main() {
    print(gcd(1, 10));
    print(gcd(2 * 34, 2 * 144));
}
