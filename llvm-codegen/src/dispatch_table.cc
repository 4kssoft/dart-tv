// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "dispatch_table.h"

#include <algorithm>
#include <cassert>
#include <limits>
#include <string>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>

#include "il_deserializer.h"
#include "llvm_codegen.h"

namespace dart_llvm {

SelectorRow::SelectorRow(size_t selector_id,
                         std::vector<size_t> cids,
                         std::vector<llvm::Constant*> table_entries)
    : selector_id_(selector_id),
      offset_(-1),
      begin_(-1),
      end_(-1),
      cids_(std::move(cids)),
      table_entries_(std::move(table_entries)) {
  assert(cids_.size() == table_entries_.size());
  if (cids_.empty()) {
    return;
  }

  for (auto cid : cids_) {
    AddToRanges(Range(cid, cid + 1));
  }
  std::sort(ranges_.begin(), ranges_.end(), RangeSizeCompare);
  std::sort(ranges_.begin() + 1, ranges_.end(), RangeBeginCompare);
}

void SelectorRow::AddToRanges(Range range) {
  begin_ = std::min(begin_, range.begin());
  end_ = std::max(end_, range.end());
  size_t i = 0;
  while (i != ranges_.size()) {
    if (!ranges_[i].Overlap(range)) {
      i++;
    } else if (ranges_[i].Contains(range)) {
      // Already covered.
      return;
    } else {
      // Expand range, remove current and continue with updated range. This
      // handles the case where two ranges may come immediately after each
      // other.
      if (ranges_[i].begin() < range.begin()) {
        range.set_begin(ranges_[i].begin());
      }
      if (range.end() < ranges_[i].end()) {
        range.set_end(ranges_[i].end());
      }
      ranges_.erase(ranges_.begin() + i);
    }
  }
  ranges_.push_back(range);
}

void SelectorRow::FillTable(std::vector<llvm::Constant*>& table) {
  for (size_t i = 0; i < cids_.size(); ++i) {
    assert(offset_ + cids_[i] < table.size());
    table[offset_ + cids_[i]] = table_entries_[i];
  }
}

int RowFitter::Fit(SelectorRow* row) {
  const Range::List& ranges = row->ranges();

  size_t slot_index;
  int offset = FindOffset(ranges, row->begin(), &slot_index);

  UpdateFreeSlots(offset, ranges, slot_index);

  MarkOffsetAsUsed(offset);
  return offset;
}

int RowFitter::FitRowWithSingleRange(SelectorRow* row) {
  assert(row->ranges().size() == 1);

  Range range = row->ranges()[0];

  size_t index = single_range_start_index_;

  while (index < free_slots_.size() - 1) {
    Range& slot = free_slots_[index];
    int offset = slot.begin() - range.begin();
    if (offset >= 0 && range.size() <= slot.size() &&
        used_offsets_.find(offset) == used_offsets_.end()) {
      // Simply move the start offset of the slot. If the slot is now full,
      // the next row will detect it and move index accordingly.
      slot.set_begin(slot.begin() + range.size());
      single_range_start_index_ = index;

      MarkOffsetAsUsed(offset);
      return offset;
    }
    index++;
  }

  single_range_start_index_ = index;

  Range& slot = free_slots_[index];
  int offset = std::max(0, slot.begin() - range.begin());
  while (used_offsets_.find(offset) != used_offsets_.end()) {
    offset++;
  }
  slot.set_begin(offset + range.end());

  MarkOffsetAsUsed(offset);
  return offset;
}

void RowFitter::MarkOffsetAsUsed(int offset) {
  assert(used_offsets_.find(offset) == used_offsets_.end());
  used_offsets_.insert(offset);
  // Keep track of the highest used offset.
  if (offset > limit_) limit_ = offset;
}

int RowFitter::FindOffset(const Range::List& ranges,
                          int min_row_index,
                          size_t* result_slot_index) {
  assert(single_range_start_index_ == 0);
  const Range largest_range = ranges[0];

  size_t index = 0;
  size_t length = free_slots_.size() - 1;
  int min_start = 0;

  while (index < length) {
    const Range slot = free_slots_[index];

    int start =
        std::max(min_start, std::max(slot.begin(), largest_range.begin()));
    int end = slot.end() - largest_range.size();

    while (start < end) {
      int offset = start - largest_range.begin();
      assert(offset >= 0);

      // At this point we expect the first (largest) range to match the 'it'
      // slot.
      assert(slot.Contains(largest_range.WithOffset(offset)));

      // Pad to guarantee unique offsets.
      if (used_offsets_.find(offset) != used_offsets_.end()) {
        start++;
        continue;
      }

      // If the largest block was the only block, we are done.
      if (ranges.size() == 1) {
        *result_slot_index = index;
        return offset;
      }

      // Found an offset where the largest range fits. Now match the
      // remaining ones.
      int displacement = MatchRemaining(offset, ranges, index);

      // Displacement is either 0 for a match, or a minimum distance to where
      // a potential match can happen.
      if (displacement == 0) {
        *result_slot_index = index;
        return offset;
      }

      start += displacement;
    }

    // TODO(ajohnsen): Perhaps check if start > end and move it accordingly
    // (to avoid min_start).
    min_start = start;

    index++;
  }

  const Range slot = free_slots_[index];
  assert(slot.end() == std::numeric_limits<int>::max());

  // If we are at end, we know it fits.
  int offset = std::max(0, slot.begin() - min_row_index);
  // Pad to guarantee unique offsets.
  while (used_offsets_.find(offset) != used_offsets_.end()) {
    offset++;
  }

  *result_slot_index = index;
  return offset;
}

int RowFitter::MatchRemaining(int offset,
                              const Range::List& ranges,
                              size_t slot_index) {
  size_t index = 1;
  size_t length = ranges.size();

  // Start by back-tracking, as second range may be before the largest.
  slot_index = MoveBackToCover(ranges[index].WithOffset(offset), slot_index);

  for (; index < length; index++) {
    const Range range = ranges[index].WithOffset(offset);

    slot_index = MoveForwardToCover(range, slot_index);
    const Range slot = free_slots_[slot_index];

    if (range.begin() < slot.begin()) return slot.begin() - range.begin();
  }

  return 0;
}

size_t RowFitter::MoveBackToCover(const Range range, size_t slot_index) {
  while (slot_index > 0 && range.IsBefore(free_slots_[slot_index])) {
    slot_index--;
  }
  return slot_index;
}

size_t RowFitter::MoveForwardToCover(const Range range, size_t slot_index) {
  while (free_slots_[slot_index].end() < range.end())
    slot_index++;
  return slot_index;
}

void RowFitter::UpdateFreeSlots(int offset,
                                const Range::List& ranges,
                                size_t slot_index) {
  for (size_t i = 0; i < ranges.size(); i++) {
    assert(slot_index < free_slots_.size());
    const Range range = ranges[i].WithOffset(offset);

    if (i > 0) {
      if (i == 1) {
        while (free_slots_[slot_index].IsAfter(range)) {
          assert(slot_index > 0);
          slot_index--;
        }
      }

      slot_index = MoveForwardToCover(range, slot_index);
    }

    // Assert that we have a valid slot.
    assert(slot_index < free_slots_.size());
    assert(free_slots_[slot_index].begin() < range.end());

    slot_index = FitInFreeSlot(range, slot_index);
  }

  for (size_t i = 0; i < free_slots_.size(); i++) {
    assert(free_slots_[i].begin() < free_slots_[i].end());
  }
}

size_t RowFitter::FitInFreeSlot(const Range range, size_t slot_index) {
  Range& slot = free_slots_[slot_index];
  assert(slot.Contains(range));
  if (slot.begin() < range.begin()) {
    if (slot.end() > range.end()) {
      free_slots_.insert(free_slots_.begin() + slot_index,
                         Range(slot.begin(), range.begin()));
      slot_index++;
      free_slots_[slot_index].set_begin(range.end());
    } else {
      slot.set_end(range.begin());
      slot_index++;
    }
  } else if (slot.end() <= range.end()) {
    assert(slot.IsSame(range));
    free_slots_.erase(free_slots_.begin() + slot_index);
  } else {
    slot.set_begin(range.end());
  }
  return slot_index;
}

Selector::Selector(std::string func_name,
                   size_t argc,
                   std::vector<std::string> named_args,
                   intptr_t arg_descriptor_id)
    : func_name_(std::move(func_name)),
      selector_(func_name_),
      argc_(argc),
      named_args_(std::move(named_args)),
      arg_descriptor_id_(arg_descriptor_id) {
  assert(argc_ >= named_args_.size());
  selector_ += "$" + std::to_string(argc);
  for (const auto& narg : named_args) {
    selector_ += "$" + narg;
  }
}


void DispatchTable::AddSelector(const Selector& selector) {
  auto it = selector_to_id_.find(selector.selector());
  if (it != selector_to_id_.end()) {
    return;
  }
  auto selector_id = selector_rows_.size();
  selector_to_id_.emplace(selector.selector(), selector_id);

  auto entries = method_table_->GetEntriesForSelector(selector);
  std::vector<size_t> cids;
  std::transform(entries.begin(), entries.end(), std::back_inserter(cids),
                 [](const auto& entry) { return std::get<size_t>(entry); });
  std::vector<llvm::Constant*> table_entries;
  for (size_t i = 0; i < cids.size(); ++i) {
    auto decl = std::get<const DartFunctionDeclaration*>(entries[i]);
    llvm::Function* target = cgm_.GetOrCreateDynamicTrampoline(decl, selector);

    auto c_target =
        llvm::ConstantExpr::getBitCast(target, cgm_.NonGCObjectPtrTy);
    auto c_selector_id = llvm::ConstantInt::get(cgm_.Int64Ty, selector_id);
    auto entry = llvm::ConstantStruct::get(cgm_.DispatchTableEntryTy,
                                           {c_selector_id, c_target});
    table_entries.push_back(entry);
  }

  selector_rows_.emplace_back(selector_id, cids, table_entries);
}

void DispatchTable::BuildTable() {
  assert(!is_built_);
  is_built_ = true;

  std::sort(selector_rows_.begin(), selector_rows_.end(), SelectorRow::Compare);

  offsets_.resize(selector_rows_.size());
  RowFitter row_fitter;
  for (auto& row : selector_rows_) {
    if (row.ranges().empty()) {
      continue;
    }
    if (row.ComputeTableSize() <= 2) {
    // TODO(sarkin): 
    // if (row.ranges().size() == 1) {
      int offset = row_fitter.FitRowWithSingleRange(&row);
      offsets_[row.selector_id()] = offset;
      row.set_offset(offset);
    } else {
      int offset = row_fitter.Fit(&row);
      offsets_[row.selector_id()] = offset;
      row.set_offset(offset);
    }
  }

  // The combined table size is limit plus enough space to guarantee
  // that looking up at the highest offset with any given receiver class
  // isn't going to be out of bounds.
  table_size_ = row_fitter.limit() + method_table_->num_cids();

  auto table_ty = llvm::ArrayType::get(cgm_.DispatchTableEntryTy, table_size_);
  // Initially all entries have -1 as selector-id, which means they
  // don't match any real selector.
  auto ctarget = llvm::ConstantPointerNull::get(cgm_.NonGCObjectPtrTy);
  auto cselector_id = llvm::ConstantInt::get(cgm_.Int64Ty, -1);
  auto entry = llvm::ConstantStruct::get(cgm_.DispatchTableEntryTy,
                                         {cselector_id, ctarget});

  std::vector<llvm::Constant*> initializer(table_size_, entry);

  for (auto& row : selector_rows_) {
    row.FillTable(initializer);
  }

  table_ = new llvm::GlobalVariable(
      cgm_.GetModule(), table_ty, true, llvm::GlobalVariable::InternalLinkage,
      llvm::ConstantArray::get(table_ty, initializer), "_DispatchTable");
}

size_t DispatchTable::GetOffset(const std::string& selector) {
  auto it = selector_to_id_.find(selector);
  assert(it != selector_to_id_.end());
  assert(offsets_.size() > it->second);
  return offsets_[it->second];
}

}  // namespace dart_llvm
