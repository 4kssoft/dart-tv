// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef LLVM_CODEGEN_H_
#define LLVM_CODEGEN_H_

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "il_deserializer.h"

#include "vm/compiler/backend/llvm_common_defs.h"
#include "vm/runtime_entry_list.h"

namespace llvm {
class ArrayType;
class BasicBlock;
class Constant;
class ConstantFolder;
class DataLayout;
class Function;
class FunctionType;
class GlobalVariable;
class IntegerType;
template <typename T, typename Inserter>
class IRBuilder;
class IRBuilderDefaultInserter;
class LLVMContext;
class Module;
class PointerType;
class StructType;
class Type;
class Value;
}  // namespace llvm

namespace dart_llvm {

struct CodegenTypeCache {
  llvm::IntegerType* Int1Ty = nullptr;
  llvm::IntegerType* Int8Ty = nullptr;
  llvm::IntegerType* Int16Ty = nullptr;
  llvm::IntegerType* Int32Ty = nullptr;
  llvm::IntegerType* Int64Ty = nullptr;

  llvm::Type* DoubleTy = nullptr;
  llvm::Type* VoidTy = nullptr;

  llvm::PointerType* ObjectPtrTy = nullptr;
  llvm::PointerType* NonGCObjectPtrTy = nullptr;
  llvm::PointerType* NonGCPtrToPtrTy = nullptr;

  llvm::ArrayType* ConstantPoolTy = nullptr;
  llvm::ArrayType* FunctionPoolTy = nullptr;

  llvm::FunctionType* FixedParamTrampolineTy = nullptr;
  llvm::ArrayType* StaticTrampolinesTy = nullptr;

  llvm::StructType* NativeArgumentsTy = nullptr;
  llvm::PointerType* NativeArgumentsPtrTy = nullptr;

  llvm::FunctionType* RuntimeFunctionTy = nullptr;
  llvm::ArrayType* RuntimeFunctionsTy = nullptr;

  llvm::PointerType* ThreadObjectTy = nullptr;
  llvm::IntegerType* FunctionIDTy = nullptr;
  llvm::PointerType* ArgumentArrayTy = nullptr;
};

class CodegenModule : public CodegenTypeCache {
 public:
  CodegenModule(llvm::Module&, const DartProgram* program);

  llvm::Module& GetModule() const { return module_; }
  llvm::LLVMContext& GetLLVMContext() const { return llvm_context_; }
  const llvm::DataLayout& GetDataLayout() const;

  void GenerateProgram();

  // TODO(sarkin): Add constant to returned pointers?
  llvm::GlobalVariable* constant_pool() const { return constant_pool_; }
  llvm::GlobalVariable* function_pool() const { return function_pool_; }
  llvm::Function* llvm_to_runtime_trampoline() const {
    return llvm_to_runtime_trampoline_;
  }

  size_t FalseOffset() const { return program_->false_offset(); }
  size_t TrueOffset() const { return program_->true_offset(); }
  size_t NullOffset() const { return program_->null_offset(); }

  intptr_t TopExitFrameInfoOffset() const {
    return program_->top_exit_frame_info_offset();
  }

  static const int kNonGCAddressSpace = 0;
  static const int kGCAddressSpace = 1;

#define DECLARE_TAG(entry) k##entry,
  enum RuntimeEntryTag { RUNTIME_ENTRY_LIST(DECLARE_TAG) kNumEntries };
#undef DECLARE_TAG

 private:
  size_t NumRuntimeFunctions() const { return RuntimeEntryTag::kNumEntries; }

  llvm::FunctionType* GetStaticFunctionType(size_t num_params);

  llvm::Constant* GetConstantInt(int64_t val,
                                 llvm::IntegerType* ty = nullptr) const;

  void GenerateRuntimeFunctionDeclarations();
  void GenerateFunctions();
  void GenerateConstants();

  void GenerateSpecialFunctions();
  void GenerateDartToLLVMTrampoline();
  void GenerateLLVMToRuntimeTrampoline();
  void GenerateFixedParamsTrampolines();
  llvm::Function* GetOrCreateFixedParamTrampoline(size_t num_params);

  static const std::string kGlobalConstantPoolName;
  static const std::string kGlobalFunctionPoolName;
  static const std::string kDartToLLVMTrampolineName;

  bool already_generated_ = false;

  llvm::Module& module_;
  llvm::LLVMContext& llvm_context_;
  const DartProgram* program_ = nullptr;

  std::unordered_map<size_t, llvm::Function*> fixed_params_trampolines_;
  llvm::GlobalVariable* static_trampolines_ = nullptr;
  llvm::GlobalVariable* constant_pool_ = nullptr;
  llvm::GlobalVariable* function_pool_ = nullptr;
  llvm::GlobalVariable* runtime_functions_ = nullptr;
  llvm::Function* llvm_to_runtime_trampoline_ = nullptr;

  static const std::unordered_map<RuntimeEntryTag, std::string>
      kRuntimeEntryTagToName;

  // TODO(sarkin): Temporary hack
 public:
  llvm::Function* unbox_ = nullptr;
  llvm::Function* box_ = nullptr;
};

// TODO(sarkin): 1 to 1 correspondence between LLVM BBs and Dart's BBs is
// assumed now, investigate whether that is the case.
// Possible issues: Dart IR *might* have multiple function entry basic blocks.
class CodegenFunction : CodegenTypeCache {
 public:
  explicit CodegenFunction(CodegenModule& cgm,
                           const DartFunction* func,
                           llvm::Function* llvm_func);

  void GenerateCode();

  // The offset of Dart function parameters in LLVM function signatures.
  static const int kDartParamOffset = 1;

 private:
  class InstructionInputExtractor {
   public:
    InstructionInputExtractor(const ILInstruction*);

    std::string NextInput();
    intptr_t NextInputAsIntPtr();
    intptr_t NextInputAsInt64();
    template <typename T>
    T NextInputAsEnum();
    size_t NumInputs() const;
    const ILInstruction* instr() const { return instr_; }

   private:
    const ILInstruction* instr_ = nullptr;
    size_t idx_ = 0;
  };

  llvm::BasicBlock* CreateBasicBlock(intptr_t id);
  llvm::BasicBlock* CreateBasicBlock(std::string id);

  void GenerateBasicBlock(const DartBasicBlock* bb, llvm::BasicBlock* llvm_bb);

  void EmitInstruction(const ILInstruction* instr);

#define DECLARE_EMIT(type, attrs)                                              \
  llvm::Value* Emit##type(InstructionInputExtractor I);
  FOR_EACH_SUPPORTED_INSTRUCTION(DECLARE_EMIT)
#undef DECLARE_EMIT
  llvm::Value* EmitRelationalOpInt64(TokenKind op,
                                     RelationalOpCid op_type,
                                     llvm::Value* left,
                                     llvm::Value* right);

  llvm::Value* EmitRelationalOpDouble(TokenKind op,
                                      RelationalOpCid op_type,
                                      llvm::Value* left,
                                      llvm::Value* right);
  llvm::Value* EmitBool(llvm::Value* value);
  llvm::Value* EmitNull();
  llvm::Value* EmitNativeArguments(intptr_t argc_tag,
                                   llvm::Value* argv,
                                   llvm::Value* ret_val);
  void EmitNullError();
  void EmitCallToRuntimeTrampoline(CodegenModule::RuntimeEntryTag tag,
                                   llvm::Value* native_args);

  llvm::BasicBlock* GetCurrentLLVMBasicBlock() const;

  llvm::Value* GetThread() const;

  void AddValue(std::string value_id, llvm::Value* value);

  void ProcessPhiIncomingValues();

  llvm::Type* GetTypeFromRepresentation(Representation representation) const;
  llvm::Value* GetValue(std::string value_id) const;
  llvm::BasicBlock* GetBlock(intptr_t block_id) const;
  llvm::Constant* GetConstantInt(int64_t val,
                                 llvm::IntegerType* ty = nullptr) const;

  bool already_generated_ = false;
  CodegenModule& cgm_;
  const DartFunction* func_ = nullptr;
  llvm::Function* llvm_func_ = nullptr;

  std::unique_ptr<
      llvm::IRBuilder<llvm::ConstantFolder, llvm::IRBuilderDefaultInserter>>
      builder_;

  std::unordered_map<std::string, llvm::Value*> values_;
  std::unordered_map<intptr_t, llvm::BasicBlock*> blocks_;
  std::unordered_map<llvm::Value*, llvm::BasicBlock*> llvm_value_to_block_;
  std::vector<const ILInstruction*> phi_instructions_;
};

}  // namespace dart_llvm

#endif  // LLVM_CODEGEN_H_
