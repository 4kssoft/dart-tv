// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.
//
// The Counting QuickPerm Algorithm (see quickperm.org/quickperm.html)
// Implementation modified to not require arrays, but rather use base-n numbers.

// Prints permutations of 0..n-1, one per line, represented as numbers in base n.
//
void perms(int n) {
    int a = 0, p = 0;
    for (int i = 0; i < n; ++i) {
        a = a * n + i;
    }
    print(a);
    int i = 1, i_base = n;
    while (i < n) {
        int p_i = (p ~/ i_base) % n;
        if (p_i < i) {
            int j = i % 2 * p_i;
            int j_base = 1;
            for (int k = 0; k < j; ++k) {
                j_base *= n;
            }

            int a_j = (a ~/ j_base) % n;
            int a_i = (a ~/ i_base) % n;

            a = a - a_j * j_base;
            a = a + a_i * j_base;

            a = a - a_i * i_base;
            a = a + a_j * i_base;

            print(a);

            p = p + i_base;
            i = 1;
            i_base = n;
        } else {
            p = p - p_i * i_base;
            i = i + 1;
            i_base = i_base * n;
        }
    }
}

void main() {
    for (int i = 1; i <= 3; ++i) {
        perms(i);
    }
}
