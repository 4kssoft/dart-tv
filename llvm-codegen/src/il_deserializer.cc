// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "il_deserializer.h"

#include <algorithm>
#include <cassert>
#include <fstream>
#include <set>
#include <sstream>

#include "dispatch_table.h"

using namespace dart_llvm;

namespace {
std::vector<std::string> SplitStringByWhitespace(std::string str) {
  std::vector<std::string> res;
  std::istringstream sin(str);
  std::string tmp;
  while (sin >> tmp) {
    res.emplace_back(std::move(tmp));
  }
  return res;
}
}  // namespace

#define DECLARE_NAME_TO_TAG_ENTRY(type, attrs) {#type, Tag::k##type},
const std::unordered_map<std::string, ILInstruction::Tag>
    ILInstruction::kNameToTag = {
        FOR_EACH_SUPPORTED_INSTRUCTION(DECLARE_NAME_TO_TAG_ENTRY)
        {"NumInstructions", Tag::kNumInstructions}
    };
#undef DECLARE_NAME_TO_TAG_ENTRY

#define DECLARE_NAME_TO_ATTRS_ENTRY(type, attrs) {#type, #attrs},
const std::unordered_map<std::string, std::string>
    ILInstruction::kNameToAttrs = {
      FOR_EACH_SUPPORTED_INSTRUCTION(DECLARE_NAME_TO_ATTRS_ENTRY)
      {"NumInstructions", "NumInstructions"}
    };
#undef DECLARE_NAME_TO_ATTRS_ENTRY

DartProgram::Ptr ILDeserializer::Deserialize(std::string filename) {
  ILDeserializer deserializer(filename);
  return deserializer.DeserializeProgram();
}

ILInstruction::Ptr ILDeserializer::DeserializeInstruction() {
  std::string instr_str = GetNextLine();
  std::vector<std::string> inputs = SplitStringByWhitespace(instr_str);

  auto instr = std::make_unique<ILInstruction>();
  bool is_value = ILInstruction::IsValue(instr_str);

  if (is_value) {
    instr->value_id_ = inputs[0];
    inputs.erase(inputs.begin());
  }

  assert(!inputs.empty());

  {
    auto it = ILInstruction::kNameToTag.find(inputs[0]);
    assert(it != ILInstruction::kNameToTag.end());
    instr->tag_ = it->second;
  }

  {
    auto it = ILInstruction::kNameToAttrs.find(inputs[0]);
    assert(it != ILInstruction::kNameToAttrs.end());
    instr->can_trigger_gc_ = !(it->second == "kNoGC");
  }

  inputs.erase(inputs.begin());
  instr->inputs_ = std::move(inputs);

  instr->is_value_ = is_value;

  return instr;
}

DartBasicBlock::Ptr ILDeserializer::DeserializeBasicBlock() {
  size_t num_instructions = ReadInt();
  auto bb = std::make_unique<DartBasicBlock>();
  bb->id_ = ReadInt();

  for (size_t i = 0; i < num_instructions; ++i) {
    bb->AddInstruction(DeserializeInstruction());
  }

  return bb;
}

void ILDeserializer::DeserializeFunctionDeclaration(
    DartFunctionDeclaration* decl) {
  decl->id_ = ReadInt();
  decl->name_ = GetNextLine();
  decl->is_static_ = ReadInt();
  decl->num_params_ = ReadInt();
  decl->num_optional_positional_ = ReadInt();
  decl->num_optional_named_ = ReadInt();
  assert(decl->num_optional_positional_ == 0 || decl->num_optional_named_ == 0);

  auto& name = decl->name_;
  std::replace(name.begin(), name.end(), '@', '$');

  if (decl->num_optional_named_) {
    for (size_t i = 0; i < decl->num_optional_named_; ++i) {
      decl->named_params_.emplace_back(GetNextLine());
    }
  }
}

DartFunction::Ptr ILDeserializer::DeserializeFunction() {
  auto func = std::make_unique<DartFunction>();

  DeserializeFunctionDeclaration(func.get());
  for (size_t i = 0; i < func->NumOptionalParameters(); ++i) {
    func->default_values_.push_back(ReadInt());
  }

  size_t num_bbs = ReadInt();
  for (size_t i = 0; i < num_bbs; ++i) {
    func->AddBasicBlock(DeserializeBasicBlock());
  }

  for (size_t i = 0; i < num_bbs && !func->can_trigger_gc_; ++i) {
    auto bb = func->BasicBlockAt(i);
    for (size_t j = 0; j < bb->NumInstructions(); ++j) {
      auto instr = bb->InstructionAt(j);
      if (instr->can_trigger_gc()) {
        func->can_trigger_gc_ = true;
        break;
      }
    }
  }

  return func;
}

DartMethodTable::Ptr ILDeserializer::DeserializeMethodTable() {
  auto table = std::make_unique<DartMethodTable>();
  table->num_cids_ = ReadInt();
  table->table_.resize(table->num_cids_);
  table->parent_cid_.resize(table->num_cids_);
  static const std::string kInvalidCid = "-1";
  for (size_t i = 0; i < table->num_cids_; ++i) {
    auto row = GetNextLine();
    if (row == kInvalidCid) {
      table->parent_cid_[i] = -1;
      continue;
    }
    std::vector<std::string> entries(SplitStringByWhitespace(row));
    const size_t kParentCidOffset = 1;
    const size_t kFirstEntryOffset = kParentCidOffset + 1;
    const size_t kEntrySize = 3;
    assert(entries.size() >= kFirstEntryOffset &&
           (entries.size() - kFirstEntryOffset) % kEntrySize == 0);
    table->parent_cid_[i] =
        std::max(0ll, std::stoll(entries[kParentCidOffset]));
    for (size_t e = kFirstEntryOffset; e < entries.size(); e += kEntrySize) {
      const std::string& name = entries[e];
      bool is_llvm_compiled = std::stol(entries[e + 1]);
      size_t id = std::stoull(entries[e + 2]);
      table->table_[i].emplace_back(name, is_llvm_compiled, id);
    }
  }
  table->FixTable();

  return table;
}

#define READ_DART_VM_CONSTANTS(name, type) program->name = ReadInt();
void ILDeserializer::DeserializeDartVMConstants(DartProgram* program) {
  DART_VM_CONSTANTS(READ_DART_VM_CONSTANTS)
}
#undef READ_DART_VM_CONSTANTS

DartClassAllocationInfo::Ptr ILDeserializer::DeserializeClassAllocationInfo() {
  std::string line = GetNextLine();
  if (line == "-1") {
    return nullptr;
  }
  auto input = SplitStringByWhitespace(line);
  assert(input.size() == 7);

  auto info = std::make_unique<DartClassAllocationInfo>();
  info->class_object_ = stoll(input[0]);
  info->is_parameterized_ = stoi(input[1]);
  info->instance_size_ = stoll(input[2]);
  info->is_allocatable_in_new_space_ = stoi(input[3]);
  info->has_fast_path_ = stoi(input[4]);
  info->tags_ = stoi(input[5]);
  info->type_arguments_field_offset_ = stoll(input[6]);

  return info;
}

DartProgram::Ptr ILDeserializer::DeserializeProgram() {
  auto prog = std::make_unique<DartProgram>();
  size_t num_functions = PopInt();

  for (size_t i = 0; i < num_functions; ++i) {
    prog->AddFunction(DeserializeFunction());
  }

  prog->false_offset_ = ReadInt();
  prog->true_offset_ = ReadInt();
  prog->null_offset_ = ReadInt();
  prog->num_constants_ = ReadInt();
  DeserializeDartVMConstants(prog.get());

  size_t num_patch_points = ReadInt();
  for (size_t i = 0; i < num_patch_points; ++i) {
    std::string patch_id = GetNextLine();
    size_t func_id = ReadInt();
    prog->static_call_patch_map_.emplace(patch_id, func_id);
  }

  prog->method_table_ = DeserializeMethodTable();
  prog->method_table_->program_ = prog.get();

  for (size_t cid = 0; cid < prog->method_table()->num_cids(); ++cid) {
    auto info = DeserializeClassAllocationInfo();
    prog->class_allocation_info_.emplace_back(std::move(info));
  }

  prog->num_dart_functions_ = ReadInt();
  for (size_t i = 0; i < prog->num_dart_functions_; ++i) {
    auto decl = std::make_unique<DartFunctionDeclaration>();
    DeserializeFunctionDeclaration(decl.get());
    prog->dart_functions_.push_back(std::move(decl));
  }

  return prog;
}

std::string ILDeserializer::GetNextLine() {
  while (current_line_ < lines_.size() && lines_[current_line_].empty()) {
    current_line_++;
  }

  assert(current_line_ < lines_.size());

  return lines_[current_line_++];
}

intptr_t ILDeserializer::PopInt() {
  assert(!lines_.empty());
  auto it = std::prev(lines_.end());
  intptr_t ans = std::stoll(*it);
  lines_.erase(it);
  return ans;
}

intptr_t ILDeserializer::ReadInt() {
  return std::stoll(GetNextLine());
}

ILDeserializer::ILDeserializer(std::string filename) {
  std::ifstream fin(filename);
  assert(fin.is_open());

  std::string tmp;
  while (getline(fin, tmp)) {
    lines_.emplace_back(std::move(tmp));
  }
}

bool ILInstruction::IsComparisonInBranch() const {
  return value_id_ == "v-1";
}

bool ILInstruction::IsValue(const std::string& instr) {
  return !instr.empty() && instr[0] == 'v';
}

std::string ILInstruction::InputAt(size_t i) const {
  assert(i >= 0 && i < inputs_.size());
  return inputs_[i];
}

void DartBasicBlock::AddInstruction(ILInstruction::Ptr instr) {
  instructions_.emplace_back(std::move(instr));
}

bool DartFunctionDeclaration::IsValidSelector(const Selector& selector) const {
  if (selector.func_name() != name_) {
    return false;
  }

  auto in_range = [](size_t i, size_t a, size_t b) -> bool {
    return i >= a && i <= b;
  };
  if (!in_range(selector.NumPositionalArgs(), NumFixedParameters(),
                NumPositionalParameters())) {
    return false;
  }

  auto is_subset = [](const auto& a, const auto& b) -> bool {
    for (const auto& el : a) {
      if (find(b.begin(), b.end(), el) == b.end()) {
        return false;
      }
    }
    return true;
  };
  return is_subset(selector.named_args(), named_params_);
}

void DartFunction::AddBasicBlock(DartBasicBlock::Ptr bb) {
  id_to_index_[bb->id()] = basic_blocks_.size();
  basic_blocks_.emplace_back(std::move(bb));
}

void DartMethodTable::FixTable() {
  for (size_t i = 0; i < num_cids_; ++i) {
    if (!IsValidCid(i)) {
      continue;
    }
    auto& old_list = table_[i];
    std::unordered_set<std::string> known_names;
    MethodList fixed_list;
    for (const auto& entry : old_list) {
      const std::string& name = std::get<std::string>(entry);
      if (known_names.find(name) != known_names.end()) {
        continue;
      }
      known_names.insert(name);
      fixed_list.push_back(entry);
    }
    old_list.swap(fixed_list);
  }
}

std::vector<DartMethodTable::DispatchTableEntry>
DartMethodTable::GetEntriesForSelector(const Selector& selector) const {
  // TODO(sarkin): Iterate all the table. Make this faster by using
  // a hash map name->methods.
  std::vector<DispatchTableEntry> entries;
  for (size_t i = 0; i < num_cids_; ++i) {
    if (!IsValidCid(i)) {
      continue;
    }
    const auto& cls = table_[i];
    for (const auto& method : cls) {
      bool is_llvm = std::get<bool>(method);
      size_t id = std::get<size_t>(method);
      auto decl = program_->FunctionDeclarationAt(is_llvm, id);
      if (decl->IsValidSelector(selector)) {
        entries.emplace_back(i, decl);
      }
    }
  }
  return entries;
}

void DartProgram::AddFunction(DartFunction::Ptr func) {
  functions_.emplace_back(std::move(func));
}

size_t DartProgram::GetFunctionIDForPatchPoint(std::string patch_point) const {
  auto it = static_call_patch_map_.find(patch_point);
  assert(it != static_call_patch_map_.end());
  return it->second;
}
