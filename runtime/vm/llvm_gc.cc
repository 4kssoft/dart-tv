// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/llvm_gc.h"

#include <algorithm>
#include <unordered_set>

#include "vm/raw_object.h"
#include "vm/stack_frame.h"
#include "vm/visitor.h"

namespace dart {

DECLARE_FLAG(bool, llvm_mode);

void LLVMStackMap::Visit(StackFrame* frame,
                         ObjectPointerVisitor* visitor) const {
  const int kDwarfRBPNum = 6;
  const int kDwarfRSPNum = 7;
  const auto num_locations = record_->num_locations;
  auto locations = &record_->first_location;

  auto assert_is_zero = [&](const StackMapRecordLocation& loc) {
    ASSERT(loc.location_type == StackMapRecordLocation::ConstantLocation);
    ASSERT(loc.offset_or_small_constant == 0);
  };
  assert_is_zero(locations[0]);
  assert_is_zero(locations[1]);
  assert_is_zero(locations[2]);

  ASSERT((num_locations - 3) % 2 == 0);
  std::unordered_set<intptr_t> bases;
  for (uint16_t i = 3; i < num_locations; i += 2) {
    auto location_type = static_cast<StackMapRecordLocation::StackMapLocation>(
        locations[i].location_type);
    ASSERT(location_type == StackMapRecordLocation::IndirectLocation);

    // Verify that the location is of the form [RBP + offset] or [RSP + offset].
    ASSERT(locations[i].dwarf_register_num == kDwarfRBPNum ||
           locations[i].dwarf_register_num == kDwarfRSPNum);
    bool is_rbp = locations[i].dwarf_register_num == kDwarfRBPNum;
    auto bp = (is_rbp) ? frame->fp() : frame->sp();
    // locations[i] is the base pointer, locations[i + 1] is the derived pointer
    // that needs to be relocated.
    ASSERT(locations[i].location_size % kWordSize == 0);
    // The relocation pair can describe multiple base/derived pointer pairs,
    // skip that case for now.
    ASSERT(locations[i].location_size == kWordSize);

    uword base_on_stack = bp + locations[i].offset_or_small_constant;
    RawObject** base = reinterpret_cast<RawObject**>(base_on_stack);

    bases.insert(base_on_stack);
    // Record the new and old base object addresses in case it's
    // moved by the visitor.
    auto original_base = *base;
    visitor->VisitPointer(base);
    auto new_base = *base;

    auto diff = new_base - original_base;
    if (diff != 0) {
      ASSERT(locations[i + 1].location_type == locations[i].location_type);
      ASSERT(locations[i + 1].dwarf_register_num ==
             locations[i].dwarf_register_num);
      uword derived_on_stack = bp + locations[i + 1].offset_or_small_constant;
      // Do nothing for pointers that were already moved by the visitor.
      if (bases.find(derived_on_stack) != bases.end()) {
        continue;
      }
      RawObject** derived = reinterpret_cast<RawObject**>(derived_on_stack);
      *derived += diff;
    }
  }
}

LLVMStackMaps::~LLVMStackMaps() {
  if (!FLAG_llvm_mode) {
    ASSERT(statepoints_ == nullptr);
    return;
  }

  // Parse is only called for the main isolate.
  if (statepoints_ == nullptr) {
    return;
  }

  delete[] statepoints_;
  for (intptr_t i = 0; i < num_records_; ++i) {
    ASSERT(stack_maps_[i] != nullptr);
    delete stack_maps_[i];
  }
  delete[] stack_maps_;
  delete[] sorted_indices_;
}

void LLVMStackMaps::Parse() {
  ASSERT(!is_parsed_);
  is_parsed_ = true;

  // Header should be set in LoadLLVMExternals.
  ASSERT(header_ != nullptr);
  ASSERT(header_->version == kVersion);

  num_records_ = header_->num_records;

  auto stack_size_records = reinterpret_cast<StackSizeRecord*>(header_ + 1);
  auto stack_map_constants = reinterpret_cast<StackMapConstant*>(
      stack_size_records + header_->num_functions);
  auto stack_map_record = reinterpret_cast<StackMapRecord*>(
      stack_map_constants + header_->num_constants);

  // TODO(sarkin): For now keep a sorted list of statepoint addresses
  // and the corresponding LLVMStackMaps. A hash map can be used instead,
  // but it's much less convenient to implement.
  statepoints_ = new uword[num_records_];
  stack_maps_ = new LLVMStackMap*[num_records_];
  for (intptr_t i = 0; i < num_records_; ++i) {
    auto instruction_offset = stack_map_record->instruction_offset;
    auto statepoint_id = stack_map_record->patch_point_id;
    statepoints_[i] =
        instruction_offset + llvm_statepoint_id_to_function_[statepoint_id];
    stack_maps_[i] = new LLVMStackMap(stack_map_record);

    // Update stack map records to point to the next record.
    {
      auto live_out_section = reinterpret_cast<StackMapRecordLiveOutSection*>(
          (&stack_map_record->first_location) +
          stack_map_record->num_locations);
      auto next_record =
          reinterpret_cast<StackMapRecord*>(live_out_section + 1);

      stack_map_record = next_record;
    }
  }
  sorted_indices_ = new intptr_t[num_records_];
  for (intptr_t i = 0; i < num_records_; ++i) {
    sorted_indices_[i] = i;
  }
  std::sort(sorted_indices_, sorted_indices_ + num_records_,
            [&](intptr_t a, intptr_t b) { return statepoints_[a] < statepoints_[b]; });
}

void LLVMStackMaps::Visit(StackFrame* frame, ObjectPointerVisitor* visitor) {
  ASSERT(num_records_ > 0);

  auto get_statepoint = [&](intptr_t i) {
    return statepoints_[sorted_indices_[i]];
  };
  auto get_stack_map = [&](intptr_t i) {
    return stack_maps_[sorted_indices_[i]];
  };

  // At least one statepoint should be valid.
  ASSERT(get_statepoint(0) <= frame->pc());

  intptr_t lo = 0;
  intptr_t hi = num_records_;

  // lo is the last statepoint <= pc.
  while (lo < hi - 1) {
    auto mid = (lo + hi) >> 1;
    if (get_statepoint(mid) <= frame->pc()) {
      lo = mid;
    } else {
      hi = mid;
    }
  }

  // lo should be the required statepoint.
  ASSERT(get_statepoint(lo) == frame->pc());

  get_stack_map(lo)->Visit(frame, visitor);
}

}  // namespace dart
