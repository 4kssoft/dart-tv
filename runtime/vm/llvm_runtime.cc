// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/llvm_runtime.h"

#include "vm/object.h"

extern "C" {

int64_t LLVMUnboxInt64(dart::RawObject* num) {
  // TODO(sarkin):
  intptr_t add = reinterpret_cast<intptr_t>(num);
  if (!(add & 1)) {
    return add >> 1;
  }

  return dart::Mint::Value(dart::Mint::RawCast(num));
}

dart::RawObject* LLVMBoxInt64(int64_t num) {
  // TODO(sarkin):
  num <<= 1;
  return reinterpret_cast<dart::RawObject*>(num);
}
}
