// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "il_deserializer.h"

#include <algorithm>
#include <cassert>
#include <fstream>
#include <sstream>

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

ILInstruction::ILInstruction(bool is_value) : is_value_(is_value) {}

#define DECLARE_NAME_TO_TAG_ENTRY(type, attrs) {#type, Tag::k##type},
const std::unordered_map<std::string, ILInstruction::Tag>
    ILInstruction::kNameToTag = {
        FOR_EACH_SUPPORTED_INSTRUCTION(DECLARE_NAME_TO_TAG_ENTRY)
        {"NumInstructions", Tag::kNumInstructions}
    };

DartProgram::Ptr ILDeserializer::Deserialize(std::string filename) {
  ILDeserializer deserializer(filename);
  return deserializer.DeserializeProgram();
}
#undef DECLARE_NAME_TO_TAG_ENTRY

ILInstruction::Ptr ILDeserializer::DeserializeInstruction() {
  std::string instr_str = GetNextLine();
  std::vector<std::string> inputs = SplitStringByWhitespace(instr_str);

  auto instr =
      std::make_unique<ILInstruction>(ILInstruction::IsValue(instr_str));
  if (instr->is_value()) {
    instr->value_id_ = inputs[0];
    inputs.erase(inputs.begin());
  }

  assert(!inputs.empty());

  auto it = ILInstruction::kNameToTag.find(inputs[0]);
  inputs.erase(inputs.begin());
  assert(it != ILInstruction::kNameToTag.end());
  instr->tag_ = it->second;

  instr->inputs_ = std::move(inputs);

  return instr;
}

DartBasicBlock::Ptr ILDeserializer::DeserializeBasicBlock() {
  size_t num_instructions = ReadInt();
  auto bb = std::make_unique<DartBasicBlock>(ReadInt());

  for (size_t i = 0; i < num_instructions; ++i) {
    bb->AddInstruction(DeserializeInstruction());
  }

  return bb;
}

DartFunction::Ptr ILDeserializer::DeserializeFunction() {
  intptr_t id = ReadInt();
  std::string name = GetNextLine() + "#" + std::to_string(id);
  // TODO(sarkin): Use is_static
  bool is_static = ReadInt();
  size_t num_params = ReadInt();
  auto func = std::make_unique<DartFunction>(id, name, is_static, num_params);

  size_t num_bbs = ReadInt();
  for (size_t i = 0; i < num_bbs; ++i) {
    func->AddBasicBlock(DeserializeBasicBlock());
  }

  return func;
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
  prog->top_exit_frame_info_offset_ = ReadInt();
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

DartBasicBlock::DartBasicBlock(intptr_t id) : id_(id) {}

void DartBasicBlock::AddInstruction(ILInstruction::Ptr instr) {
  instructions_.emplace_back(std::move(instr));
}

DartFunction::DartFunction(intptr_t id,
                           std::string name,
                           bool is_static,
                           size_t num_params)
    : id_(id), name_(name), is_static_(is_static), num_params_(num_params) {}

void DartFunction::AddBasicBlock(DartBasicBlock::Ptr bb) {
  id_to_index_[bb->id()] = basic_blocks_.size();
  basic_blocks_.emplace_back(std::move(bb));
}

void DartProgram::AddFunction(DartFunction::Ptr func) {
  functions_.emplace_back(std::move(func));
}
