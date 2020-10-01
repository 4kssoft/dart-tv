// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.
//
// Ackermann function implementation.
//
// Expected output:
// 1
// 2
// 3
// 4
// ...
// 61
// 125
// 253
//
int ack(int m, int n) {
    if (m == 0) {
        return n + 1;
    } else if (n == 0) {
        return ack(m - 1, 1);
    } else {
        return ack(m - 1, ack(m, n - 1));
    }
}

void main() {
    for (int x = 0; x <= 3; ++x) {
        for (int y = 0; y <= 5; ++y) {
            print(ack(x, y));
        }
    }
}
