// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef RUNTIME_VM_LLVM_RUNTIME_H_
#define RUNTIME_VM_LLVM_RUNTIME_H_

#include "vm/raw_object.h"

extern "C" {
int64_t LLVMUnboxInt64(dart::RawObject* num);

dart::RawObject* LLVMBoxInt64(int64_t num);
}

#endif  // RUNTIME_VM_LLVM_RUNTIME_H_
