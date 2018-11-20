// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef DISPATCH_TABLE_H_
#define DISPATCH_TABLE_H_

#include <cassert>
#include <limits>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace llvm {
class Constant;
class GlobalVariable;
class Value;
}  // namespace llvm

namespace dart_llvm {

class Range {
 public:
  using List = std::vector<Range>;

  Range(int begin, int end) : begin_(begin), end_(end) { assert(end > begin); }

  int begin() const { return begin_; }
  void set_begin(int value) { begin_ = value; }

  int end() const { return end_; }
  void set_end(int value) { end_ = value; }

  int size() const { return end_ - begin_; }

  Range WithOffset(int offset) const {
    return Range(begin_ + offset, end_ + offset);
  }

  bool IsSame(const Range other) const {
    return end() == other.end() && begin() == other.begin();
  }

  bool IsBefore(const Range other) const { return end() < other.begin(); }

  bool IsAfter(const Range other) const { return begin() > other.end(); }

  bool Overlap(const Range other) const {
    return !IsBefore(other) && !IsAfter(other);
  }

  bool ContainsBeginOf(const Range other) const {
    return begin() <= other.begin() && other.begin() <= end();
  }

  bool ContainsEndOf(const Range other) const {
    return begin() <= other.end() && other.end() <= end();
  }

  bool Contains(const Range other) const {
    return ContainsBeginOf(other) && ContainsEndOf(other);
  }

 private:
  int begin_;
  int end_;
};

class SelectorRow {
 public:
  SelectorRow(size_t selector_id,
              std::vector<size_t> cids,
              std::vector<llvm::Constant*> table_entries);

  size_t selector_id() const { return selector_id_; }

  int begin() const { return begin_; }

  int end() const { return end_; }

  int offset() const { return offset_; }

  void set_offset(int value) { offset_ = value; }

  int ComputeTableSize() const { return end_ - begin_; }

  static bool RangeSizeCompare(const Range& a, const Range& b) {
    return a.size() > b.size();
  }

  static bool RangeBeginCompare(const Range& a, const Range& b) {
    return a.begin() < b.begin();
  }

  static bool Compare(const SelectorRow& a, const SelectorRow& b) {
    int a_size = a.ComputeTableSize();
    int b_size = b.ComputeTableSize();

    // Sort by decreasing sizes (first) and decreasing begin index.
    // According to the litterature, this leads to fewer holes and
    // faster row offset computation.
    return (a_size == b_size) ? a.begin() > b.begin() : a_size > b_size;
  }

  const Range::List& ranges() const { return ranges_; }

  void FillTable(std::vector<llvm::Constant*>& table);

 private:
  void AddToRanges(Range range);

  size_t selector_id_;
  int offset_;

  Range::List ranges_;

  // All used entries in this row are in the [begin, end) interval.
  int begin_;
  int end_;

  std::vector<size_t> cids_;
  std::vector<llvm::Constant*> table_entries_;
};

class RowFitter {
 public:
  RowFitter() : single_range_start_index_(0), limit_(0) {
    // TODO(ajohnsen): Let the last range be implicit?
    free_slots_.push_back(Range(0, std::numeric_limits<int>::max()));
  }

  int limit() const { return limit_; }

  int Fit(SelectorRow* row);

  int FitRowWithSingleRange(SelectorRow* row);

 private:
  void MarkOffsetAsUsed(int offset);

  int FindOffset(const Range::List& ranges,
                 int min_row_index,
                 size_t* result_slot_index);

  int MatchRemaining(int offset, const Range::List& ranges, size_t slot_index);

  size_t MoveBackToCover(const Range range, size_t slot_index);

  size_t MoveForwardToCover(const Range range, size_t slot_index);

  void UpdateFreeSlots(int offset,
                       const Range::List& ranges,
                       size_t slot_index);

  size_t FitInFreeSlot(const Range range, size_t slot_index);

  std::unordered_set<intptr_t> used_offsets_;
  Range::List free_slots_;
  int single_range_start_index_;
  int limit_;
};

class DartMethodTable;
class CodegenModule;

class Selector {
 public:
  explicit Selector(std::string func_name,
                    size_t argc,
                    std::vector<std::string> named_args,
                    intptr_t arg_descriptor_id);

  std::string selector() const { return selector_; }
  size_t argc() const { return argc_; }
  const std::string& func_name() const { return func_name_; }
  const std::vector<std::string>& named_args() const { return named_args_; }
  intptr_t arg_descriptor_id() const { return arg_descriptor_id_; }

  size_t NumPositionalArgs() const { return argc_ - named_args_.size(); }

 private:
  std::string func_name_;
  std::string selector_;
  size_t argc_ = 0;
  std::vector<std::string> named_args_;
  intptr_t arg_descriptor_id_ = -1;
};

class DispatchTable {
 public:
  using Ptr = std::unique_ptr<DispatchTable>;

  DispatchTable(CodegenModule& cgm, const DartMethodTable* dispatch_table)
      : cgm_(cgm), method_table_(dispatch_table) {}

  void AddSelector(const Selector& selector);

  size_t GetOffset(const std::string& selector);

  void BuildTable();

  llvm::GlobalVariable* dispatch_table() const { return table_; }

 private:
  CodegenModule& cgm_;
  const DartMethodTable* method_table_;

  bool is_built_ = false;
  size_t table_size_ = 0;
  llvm::GlobalVariable* table_ = nullptr;
  std::unordered_map<std::string, size_t> selector_to_id_;
  std::vector<size_t> offsets_;
  std::vector<SelectorRow> selector_rows_;
};

}  // namespace dart_llvm

#endif  // DISPATCH_TABLE_
