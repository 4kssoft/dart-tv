// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef IL_DESERIALIZER_H_
#define IL_DESERIALIZER_H_

#include <cassert>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "vm/compiler/backend/llvm_common_defs.h"

namespace dart_llvm {

class DartProgram;
class Selector;

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

  intptr_t id() const { return id_; }

  size_t NumInstructions() const { return instructions_.size(); }
  const ILInstruction* InstructionAt(size_t i) const {
    return instructions_[i].get();
  }

 private:
  void AddInstruction(ILInstruction::Ptr instr);

  intptr_t id_ = 0;
  std::vector<ILInstruction::Ptr> instructions_;

  friend class ILDeserializer;
};

class DartFunctionDeclaration {
 public:
  using Ptr = std::unique_ptr<DartFunctionDeclaration>;

  virtual ~DartFunctionDeclaration() {}

  intptr_t id() const { return id_; }
  std::string name() const { return name_; }
  bool is_static() const { return is_static_; }
  virtual bool is_llvm() const { return false; }
  size_t num_params() const { return num_params_; }

  size_t NumOptionalParameters() const {
    return std::max(num_optional_positional_, num_optional_named_);
  }

  size_t NumFixedParameters() const {
    assert(NumOptionalParameters() <= num_params_);
    return num_params_ - NumOptionalParameters();
  }

  size_t NumPositionalParameters() const {
    assert(num_optional_named_ <= num_params_);
    return num_params_ - num_optional_named_;
  }

  bool HasNamedParameters() const { return num_optional_named_; }

  std::string NamedParameterAt(size_t i) const {
    assert(i < named_params_.size());
    return named_params_[i];
  }

  bool IsValidSelector(const Selector& selector) const;

 private:
  intptr_t id_ = 0;
  std::string name_;
  bool is_static_ = false;
  size_t num_params_ = 0;
  size_t num_optional_positional_ = 0;
  size_t num_optional_named_ = 0;
  std::vector<std::string> named_params_;

  friend class ILDeserializer;
};

class DartFunction : public DartFunctionDeclaration {
 public:
  using Ptr = std::unique_ptr<DartFunction>;

  virtual ~DartFunction() {}

  virtual bool is_llvm() const { return true; }

  size_t NumBasicBlocks() const { return basic_blocks_.size(); }
  const DartBasicBlock* BasicBlockAt(size_t i) const {
    return basic_blocks_[i].get();
  }

  size_t DefaultParamValueAt(size_t i) const {
    assert(i < default_values_.size());
    return default_values_[i];
  }

 private:
  void AddBasicBlock(DartBasicBlock::Ptr bb);

  std::vector<intptr_t> default_values_;

  std::vector<DartBasicBlock::Ptr> basic_blocks_;
  std::unordered_map<intptr_t, size_t> id_to_index_;

  friend class ILDeserializer;
};

class DartMethodTable {
 public:
  using Ptr = std::unique_ptr<DartMethodTable>;
  // (cid, decl)
  using DispatchTableEntry = std::pair<size_t, const DartFunctionDeclaration*>;

  bool IsValidCid(size_t cid) const { return parent_cid_[cid] >= 0; }

  size_t num_cids() const { return num_cids_; }

  std::vector<DispatchTableEntry> GetEntriesForSelector(
      const Selector& selector) const;

 private:
  // (name, IsLLVMCompiled, id)
  using MethodDescription = std::tuple<std::string, bool, size_t>;
  using MethodList = std::vector<MethodDescription>;

  void FixTable();

  size_t num_cids_ = 0;
  std::vector<MethodList> table_;
  std::vector<intptr_t> parent_cid_;

  const DartProgram* program_;

  friend class ILDeserializer;
};

class DartClassAllocationInfo {
 public:
  using Ptr = std::unique_ptr<DartClassAllocationInfo>;

  intptr_t class_object() const { return class_object_; }
  bool is_parameterized() const { return is_parameterized_; }
  intptr_t instance_size() const { return instance_size_; }
  bool is_allocatable_in_new_space() const {
    return is_allocatable_in_new_space_;
  }
  bool has_fast_path() const { return has_fast_path_; }
  int32_t tags() const { return tags_; }
  intptr_t type_arguments_field_offset() const {
    return type_arguments_field_offset_;
  }

 private:
  intptr_t class_object_ = 0;
  bool is_parameterized_ = false;
  intptr_t instance_size_ = 0;
  bool is_allocatable_in_new_space_ = false;
  bool has_fast_path_ = false;
  int32_t tags_ = 0;
  intptr_t type_arguments_field_offset_ = 0;

  friend class ILDeserializer;
};

#define DART_VM_CONSTANTS(M)                                                   \
  M(kWordSize, int)                                                            \
  M(kWordSizeLog2, int)                                                        \
  M(kObjectAlignment, int)                                                     \
  M(kObjectAlignmentLog2, int)                                                 \
  M(kThreadTopExitFrameInfoOffset, intptr_t)                                   \
  M(kThreadTopOffset, intptr_t)                                                \
  M(kThreadEndOffset, intptr_t)                                                \
  M(kThreadPredefinedSymbolsAddressOffset, intptr_t)                           \
  M(kInstanceTagsOffset, intptr_t)                                             \
  M(kInstanceNextFieldOffset, intptr_t)                                        \
  M(kFieldStaticValueOffset, intptr_t)                                         \
  M(kFieldGuardedCidOffset, intptr_t)                                          \
  M(kFieldIsNullableOffset, intptr_t)                                          \
  M(kFieldKindBitsOffset, intptr_t)                                            \
  M(kFieldUnboxingCandidateBit, int)                                           \
  M(kSymbolNullCharCodeSymbolOffset, int)                                      \
  M(kArrayMaxNewSpaceElements, intptr_t)                                       \
  M(kRawArraySize, intptr_t)                                                   \
  M(kRawObjectMaxSizeTag, intptr_t)                                            \
  M(kRawObjectSizeTagPos, int)                                                 \
  M(kRawArrayTags, int)                                                        \
  M(kArrayTagsOffset, intptr_t)                                                \
  M(kArrayTypeArgumentsOffset, intptr_t)                                       \
  M(kArrayLengthOffset, intptr_t)                                              \
  M(kSmiCid, int)                                                              \
  M(kMintCid, int)                                                             \
  M(kDoubleCid, int)                                                           \
  M(kFloat32x4Cid, int)                                                        \
  M(kFloat64x2Cid, int)                                                        \
  M(kNullCid, int)                                                             \
  M(kBoolCid, int)                                                             \
  M(kArrayCid, int)                                                            \
  M(kImmutableArrayCid, int)                                                   \
  M(kTypedDataFloat32ArrayCid, int)                                            \
  M(kTypedDataFloat64ArrayCid, int)                                            \
  M(kTypedDataInt8ArrayCid, int)                                               \
  M(kTypedDataUint8ArrayCid, int)                                              \
  M(kTypedDataUint8ClampedArrayCid, int)                                       \
  M(kExternalTypedDataUint8ArrayCid, int)                                      \
  M(kExternalTypedDataUint8ClampedArrayCid, int)                               \
  M(kOneByteStringCid, int)                                                    \
  M(kExternalOneByteStringCid, int)                                            \
  M(kTypedDataInt16ArrayCid, int)                                              \
  M(kTypedDataUint16ArrayCid, int)                                             \
  M(kTypedDataInt32ArrayCid, int)                                              \
  M(kTypedDataUint32ArrayCid, int)                                             \
  M(kTypedDataInt64ArrayCid, int)                                              \
  M(kTypedDataUint64ArrayCid, int)                                             \
  M(kTwoByteStringCid, int)                                                    \
  M(kExternalTwoByteStringCid, int)                                            \
  M(kMintValueOffset, int)                                                     \
  M(kDoubleValueOffset, int)                                                   \
  M(kFloat32x4ValueOffset, int)                                                \
  M(kFloat64x2ValueOffset, int)

struct DartVMConstants {
#define DEFINE_DART_VM_CONSTANTS(name, type) type name;
  DART_VM_CONSTANTS(DEFINE_DART_VM_CONSTANTS)
#undef DEFINE_DART_VM_CONSTANTS 
  friend class ILDeserializer;
};

class DartProgram : public DartVMConstants {
 public:
  using Ptr = std::unique_ptr<DartProgram>;

  intptr_t num_constants() const { return num_constants_; }
  size_t false_offset() const { return false_offset_; }
  size_t true_offset() const { return true_offset_; }
  size_t null_offset() const { return null_offset_; }
  intptr_t num_dart_functions() const { return num_dart_functions_; }
  const DartMethodTable* method_table() const { return method_table_.get(); }

  size_t NumFunctions() const { return functions_.size(); }
  const DartFunction* FunctionAt(size_t i) const { return functions_[i].get(); }
  const DartFunctionDeclaration* FunctionDeclarationAt(bool is_llvm,
                                                       size_t i) const {
    return (is_llvm) ? functions_[i].get() : dart_functions_[i].get();
  }

  size_t GetFunctionIDForPatchPoint(std::string patch_point) const;

  const DartClassAllocationInfo* GetClassAllocationInfo(size_t cid) const {
    assert(cid < class_allocation_info_.size());
    return class_allocation_info_[cid].get();
  }

 private:
  void AddFunction(DartFunction::Ptr func);

  intptr_t num_constants_ = 0;
  size_t false_offset_ = 0;
  size_t true_offset_ = 0;
  size_t null_offset_ = 0;
  size_t num_dart_functions_ = 0;
  DartMethodTable::Ptr method_table_ = nullptr;
  std::vector<DartFunction::Ptr> functions_;
  std::vector<DartFunctionDeclaration::Ptr> dart_functions_;
  std::unordered_map<std::string, size_t> static_call_patch_map_;
  std::vector<DartClassAllocationInfo::Ptr> class_allocation_info_;

  friend class ILDeserializer;
};

class ILDeserializer {
 public:
  static DartProgram::Ptr Deserialize(std::string filename);

 private:
  explicit ILDeserializer(std::string filename);

  ILInstruction::Ptr DeserializeInstruction();
  DartBasicBlock::Ptr DeserializeBasicBlock();
  void DeserializeFunctionDeclaration(DartFunctionDeclaration* decl);
  DartFunction::Ptr DeserializeFunction();
  DartMethodTable::Ptr DeserializeMethodTable();
  void DeserializeDartVMConstants(DartProgram* program);
  DartClassAllocationInfo::Ptr DeserializeClassAllocationInfo();
  DartProgram::Ptr DeserializeProgram();

  intptr_t ReadInt();
  intptr_t PopInt();
  std::string GetNextLine();

  size_t current_line_ = 0;
  std::vector<std::string> lines_;
};

}  // namespace dart_llvm

#endif // IL_DESERIALIZER_H_
