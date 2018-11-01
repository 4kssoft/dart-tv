// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef IL_DESERIALIZER_H_
#define IL_DESERIALIZER_H_

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "vm/compiler/backend/llvm_common_defs.h"

namespace dart_llvm {

class ILInstruction {
 public:
  using Ptr = std::unique_ptr<ILInstruction>;

#define DECLARE_TAG(type, attrs) k##type,
  enum class Tag {
    kInvalidTag,
    FOR_EACH_SUPPORTED_INSTRUCTION(DECLARE_TAG)
    kNumInstructions
  };
#undef DECLARE_TAG

  explicit ILInstruction(bool is_value);

  static bool IsValue(const std::string& instr);

  bool IsComparisonInBranch() const;

  Tag tag() const { return tag_; }
  bool is_value() const { return is_value_; }
  std::string value_id() const { return value_id_; }

  // TODO(sarkin): Currently there's a huge mess with size_t and intptr_t.
  // Use size_t whenever the value is an index in a STL container,
  // otherwise use intptr_t.
  size_t NumInputs() const { return inputs_.size(); }

  std::string InputAt(size_t i) const;

 private:
  bool is_value_ = false;
  std::string value_id_;
  Tag tag_ = Tag::kInvalidTag;
  std::vector<std::string> inputs_;

  static const std::unordered_map<std::string, Tag> kNameToTag;
  friend class ILDeserializer;
};

class DartBasicBlock {
 public:
  using Ptr = std::unique_ptr<DartBasicBlock>;

  explicit DartBasicBlock(intptr_t id);

  intptr_t id() const { return id_; }

  void AddInstruction(ILInstruction::Ptr instr);
  size_t NumInstructions() const { return instructions_.size(); }
  const ILInstruction* InstructionAt(size_t i) const {
    return instructions_[i].get();
  }

 private:
  intptr_t id_;
  std::vector<ILInstruction::Ptr> instructions_;
};

class DartFunction {
 public:
  using Ptr = std::unique_ptr<DartFunction>;

  explicit DartFunction(intptr_t id,
                        std::string name,
                        bool is_static,
                        size_t num_params);

  intptr_t id() const { return id_; }
  std::string name() const { return name_; }
  bool is_static() const { return is_static_; }
  size_t num_params() const { return num_params_; }

  void AddBasicBlock(DartBasicBlock::Ptr bb);
  size_t NumBasicBlocks() const { return basic_blocks_.size(); }
  const DartBasicBlock* BasicBlockAt(size_t i) const {
    return basic_blocks_[i].get();
  }

 private:
  intptr_t id_;
  std::string name_;
  bool is_static_;
  size_t num_params_;
  std::vector<DartBasicBlock::Ptr> basic_blocks_;
  std::unordered_map<intptr_t, size_t> id_to_index_;
};

class DartProgram {
 public:
  using Ptr = std::unique_ptr<DartProgram>;

  intptr_t num_constants() const { return num_constants_; }
  size_t false_offset() const { return false_offset_; }
  size_t true_offset() const { return true_offset_; }
  size_t null_offset() const { return null_offset_; }
  intptr_t top_exit_frame_info_offset() const {
    return top_exit_frame_info_offset_;
  }

  void AddFunction(DartFunction::Ptr func);
  size_t NumFunctions() const { return functions_.size(); }
  const DartFunction* FunctionAt(size_t i) const { return functions_[i].get(); }

 private:
  intptr_t num_constants_ = 0;
  size_t false_offset_ = 0;
  size_t true_offset_ = 0;
  size_t null_offset_ = 0;
  intptr_t top_exit_frame_info_offset_ = 0;
  std::vector<DartFunction::Ptr> functions_;

  friend class ILDeserializer;
};

class ILDeserializer {
 public:
  static DartProgram::Ptr Deserialize(std::string filename);

 private:
  explicit ILDeserializer(std::string filename);

  ILInstruction::Ptr DeserializeInstruction();
  DartBasicBlock::Ptr DeserializeBasicBlock();
  DartFunction::Ptr DeserializeFunction();
  DartProgram::Ptr DeserializeProgram();

  intptr_t ReadInt();
  intptr_t PopInt();
  std::string GetNextLine();

  size_t current_line_ = 0;
  std::vector<std::string> lines_;
};

}  // namespace dart_llvm

#endif // IL_DESERIALIZER_H_
