// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef RUNTIME_VM_LLVM_GC_H_
#define RUNTIME_VM_LLVM_GC_H_

#include <cstdint>

#include "platform/allocation.h"
#include "platform/globals.h"

namespace dart {

class ObjectPointerVisitor;
class RawObject;
class StackFrame;

struct StackMapHeader {
  uint8_t version;
  uint8_t reserved_1;
  uint16_t reserved_2;
  uint32_t num_functions;
  uint32_t num_constants;
  uint32_t num_records;
};

struct StackSizeRecord {
  uint64_t function_address;
  uint64_t stack_size;
  uint64_t record_count;
};

struct StackMapConstant {
  uint64_t large_constant;
};

struct StackMapRecordLocation {
  typedef enum {
    RegisterLocation = 1,
    DirectLocation,
    IndirectLocation,
    ConstantLocation,
    ConstantIndexLocation
  } StackMapLocation;

  uint8_t location_type;  // Actually StackMapLocation enum.
  uint8_t reserved_1;     // Expected to be 0;
  uint16_t location_size;
  uint16_t dwarf_register_num;
  uint16_t reserved_2;  // Expected to be 0.
  int32_t offset_or_small_constant;
};

struct StackMapRecord {
  uint64_t patch_point_id;
  uint32_t instruction_offset;
  uint16_t reserved;
  uint16_t num_locations;
  StackMapRecordLocation first_location;  // There may be multiple.
};

struct StackMapRecordLiveOut {
  uint16_t dwarf_register_number;
  uint8_t reserved;
  uint8_t size_in_bytes;
};

struct StackMapRecordLiveOutSection {
  uint32_t padding_1;
  uint16_t padding_2;
  uint16_t num_live_outs;
  // StackMapRecordLiveOut first_live_out;  // There may be multiple.
  uint32_t padding_3;
};

struct StackMapEntry {
  int stack_size;
  StackMapRecord* map;
};

class LLVMStackMap {
 public:
  explicit LLVMStackMap(StackMapRecord* record) : record_(record) {}
  void Visit(StackFrame* frame, ObjectPointerVisitor* visitor) const;

 private:
  const StackMapRecord* record_ = nullptr;
};

class LLVMStackMaps {
 public:
  LLVMStackMaps() {}
  ~LLVMStackMaps();

  void Parse();
  void Visit(StackFrame* frame, ObjectPointerVisitor* visitor);

  void set_llvm_statepoint_id_to_function(const uword* ptr) {
    llvm_statepoint_id_to_function_ = ptr;
  }

  void set_header(StackMapHeader* header) { header_ = header; }

 private:
  bool is_parsed_ = false;

  StackMapHeader* header_;
  intptr_t num_records_ = 0;
  uword* statepoints_ = nullptr;
  LLVMStackMap** stack_maps_ = nullptr;
  intptr_t* sorted_indices_ = nullptr;

  static const intptr_t kVersion = 3;
  const uword* llvm_statepoint_id_to_function_;
};

}  // namespace dart

#endif  // RUNTIME_VM_LLVM_GC_H_
