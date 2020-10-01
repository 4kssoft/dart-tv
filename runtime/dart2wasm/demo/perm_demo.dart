// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.
//
// The Counting QuickPerm Algorithm (see quickperm.org/quickperm.html)
// Implementation modified to not need require arrays, but rather base-n numbers.
//
// Expected output:
// 0
//
// 1 0
// 0 1
//
// 2 1 0
// 1 2 0
// 0 2 1
// 2 0 1
// 1 0 2
// 0 1 2
//
// 3 2 1 0
// 2 3 1 0
// 1 3 2 0
// 3 1 2 0
// 2 1 3 0
// 1 2 3 0
// 0 2 3 1
// 2 0 3 1
// 3 0 2 1
// 0 3 2 1
// 2 3 0 1
// 3 2 0 1
// 1 2 0 3
// 2 1 0 3
// 0 1 2 3
// 1 0 2 3
// 2 0 1 3
// 0 2 1 3
// 3 2 1 0
// 2 3 1 0
// 1 3 2 0
// 3 1 2 0
// 2 1 3 0
// 1 2 3 0
//
// 4 3 2 1 0
// ...

// Helpers for low-level interaction with print imported from JS.
void print_number(int n) {
    print(n);
}
// Space and newline are hardcoded as two arbitrary negative integers.
void print_space() {
    print(-10001);
}
void print_newline() {
    print(-10000);
}
void print_number_digits(int n, int length, int base) {
    for (int i = 0; i < length; ++i) {
        print_number(n % base);
        if (i + 1 < length) {
            print_space();
        }
        n ~/= base;
    }
    print_newline();
}

// Prints permutations of 0..n-1, one per line.
void perms(int n) {
    int a = 0, p = 0;
    for (int i = 0; i < n; ++i) {
        a = a * n + i;
    }
    print_number_digits(a, n, n);
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

            print_number_digits(a, n, n);

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
    for (int i = 1; i <= 5; ++i) {
        perms(i); print_newline();
    }
}

