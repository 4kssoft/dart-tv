// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef LLVM_CODEGEN_H_
#define LLVM_CODEGEN_H_

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "dispatch_table.h"
#include "il_deserializer.h"
#include "util.h"

#include "vm/compiler/backend/llvm_common_defs.h"

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
class LoadInst;
class Module;
class PointerType;
class StructType;
class Type;
class Value;
class VectorType;
}  // namespace llvm

namespace dart_llvm {

struct CodegenTypeCache {
  llvm::IntegerType* IntTy = nullptr;
  llvm::IntegerType* Int1Ty = nullptr;
  llvm::IntegerType* Int8Ty = nullptr;
  llvm::IntegerType* Int16Ty = nullptr;
  llvm::IntegerType* Int32Ty = nullptr;
  llvm::IntegerType* Int64Ty = nullptr;

  llvm::Type* FloatTy = nullptr;
  llvm::Type* DoubleTy = nullptr;
  llvm::Type* VoidTy = nullptr;

  llvm::VectorType* Int32x4Ty = nullptr;

  llvm::VectorType* Float32x4Ty = nullptr;
  llvm::VectorType* Float64x2Ty = nullptr;

  llvm::PointerType* ObjectPtrTy = nullptr;
  llvm::PointerType* PtrToPtrTy = nullptr;
  llvm::PointerType* NonGCObjectPtrTy = nullptr;
  llvm::PointerType* NonGCPtrToPtrTy = nullptr;

  llvm::ArrayType* ConstantPoolTy = nullptr;
  llvm::ArrayType* FunctionPoolTy = nullptr;
  llvm::ArrayType* DartFunctionPoolTy = nullptr;

  llvm::FunctionType* LLVMToDartTrampolineTy = nullptr;

  llvm::FunctionType* FixedParamTrampolineTy = nullptr;
  llvm::ArrayType* StaticTrampolinesTy = nullptr;

  llvm::StructType* NativeArgumentsTy = nullptr;
  llvm::PointerType* NativeArgumentsPtrTy = nullptr;

  llvm::FunctionType* RuntimeFunctionTy = nullptr;
  llvm::ArrayType* RuntimeFunctionsTy = nullptr;

  llvm::PointerType* ThreadObjectTy = nullptr;
  llvm::IntegerType* FunctionIDTy = nullptr;
  llvm::PointerType* StackArrayTy = nullptr;

  llvm::StructType* DispatchTableEntryTy = nullptr;
  llvm::PointerType* DispatchTableTy = nullptr;
};

struct AddStatepointIDsToCallSites;

class CodegenModule : public CodegenTypeCache, public DartVMConstants {
 public:
  CodegenModule(llvm::Module&, const DartProgram* program);

  llvm::Module& GetModule() const { return module_; }
  llvm::LLVMContext& GetLLVMContext() const { return llvm_context_; }
  const llvm::DataLayout& GetDataLayout() const;

  void GenerateProgram();

  llvm::GlobalVariable* constant_pool() const { return constant_pool_; }
  llvm::GlobalVariable* function_pool() const { return function_pool_; }
  llvm::GlobalVariable* dart_function_pool() const {
    return dart_function_pool_;
  }

  llvm::Function* llvm_to_runtime_trampoline() const {
    return llvm_to_runtime_trampoline_;
  }

  llvm::GlobalVariable* llvm_to_dart_trampoline() const {
    return llvm_to_dart_trampoline_;
  }

  llvm::Function* read_sp() const { return read_sp_; }

  llvm::Function* create_array_stub() const {
    return create_array_stub_;
  }

  llvm::Function* write_barrier() const { return write_barrier_; }

  llvm::GlobalVariable* GetLLVMDispatchTable() const {
    auto table = dispatch_table_->dispatch_table();
    return (table != nullptr) ? table : temp_dispatch_table_;
  }

  const DartClassAllocationInfo* GetClassAllocationInfo(size_t cid) {
    return program_->GetClassAllocationInfo(cid);
  }

  llvm::Function* no_such_method() const { return no_such_method_; }

  size_t GetFunctionIDByPatchPoint(const std::string& patch_point) const;
  llvm::Function* GetFunctionByPatchPoint(const std::string& patch_point) const;
  size_t GetDartFunctionIndexByPatchPoint(const std::string& patch_point) const;

  size_t FalseOffset() const { return program_->false_offset(); }
  size_t TrueOffset() const { return program_->true_offset(); }
  size_t NullOffset() const { return program_->null_offset(); }

  llvm::Function* GetOrCreateDartCallTrampoline(size_t function_id,
                                                const Selector& selector);

  llvm::Function* GetOrCreateHandleOptionalParamsTrampoline(
      size_t function_id,
      const Selector& selector);

  llvm::Function* GetOrCreateDynamicTrampoline(
      const DartFunctionDeclaration* target,
      const Selector& selector);

  llvm::Function* GetOrCreateClassAllocationStub(size_t cid);

  llvm::FunctionType* GetStaticFunctionType(size_t num_params);

  static const int kNonGCAddressSpace = 0;
#ifdef TARGET_WASM
  static const int kGCAddressSpace = 0;
#else
  static const int kGCAddressSpace = 1;
#endif

#define DECLARE_TAG(entry) k##entry,
  enum RuntimeEntryTag { LLVM_RUNTIME_ENTRY_LIST(DECLARE_TAG) kNumEntries };
#undef DECLARE_TAG

  llvm::Function* new_object() const { return new_object_; }
  llvm::Function* GetOrCreateNewObject();

 private:
  friend struct AddStatepointIDsToCallSites;

  size_t NumRuntimeFunctions() const { return RuntimeEntryTag::kNumEntries; }

  llvm::Function* GetFunctionByID(size_t id) const;
  const DartFunction* GetDartFunctionByID(size_t id) const;

  void AddStatepoint(llvm::Function* func);
  void CreateStatepointIDToFunctionTable();

  void GenerateRuntimeFunctionDeclarations();
  void GenerateFunctions();
  void InitializeGlobals();

  void GenerateSpecialFunctions();
  void GenerateDartToLLVMTrampoline();
  void GenerateReadStackPointer();
  void GenerateLLVMToRuntimeTrampoline();
  void GenerateFixedParamsTrampolines();
  void GenerateCreateArrayStub();
  void GenerateWriteBarrier();
  void GenerateGetStackMaps();

  void OptimizeModule();

  llvm::Function* GetOrCreateFixedParamTrampoline(size_t num_params);

  llvm::Constant* GetConstantInt(int64_t val, llvm::IntegerType* ty = nullptr) const;

  static const std::string kGCStrategyName;

  static const std::string kGlobalConstantPoolName;
  static const std::string kGlobalFunctionPoolName;
  static const std::string kGlobalDartFunctionPoolName;
  static const std::string kGlobalLLVMToDartTrampolineName;
  static const std::string kDartToLLVMTrampolineName;

  bool is_generated_ = false;

  llvm::Module& module_;
  llvm::LLVMContext& llvm_context_;
  const DartProgram* program_ = nullptr;
  std::unique_ptr<DispatchTable> dispatch_table_ = nullptr;

  std::vector<llvm::Function*> statepoint_id_to_func_;
  std::unordered_map<size_t, llvm::Function*> function_by_id_;
  std::unordered_map<size_t, llvm::Function*> fixed_params_trampolines_;
  std::unordered_map<std::pair<size_t, std::string>, llvm::Function*, PairHash>
      optional_param_trampoline_cache_;
  std::unordered_map<std::pair<size_t, std::string>, llvm::Function*, PairHash>
      dart_call_trampoline_cache_;
  std::unordered_map<size_t, llvm::Function*> class_allocation_stub_cache_;
  llvm::GlobalVariable* static_trampolines_ = nullptr;
  llvm::GlobalVariable* constant_pool_ = nullptr;
  llvm::GlobalVariable* function_pool_ = nullptr;
  llvm::GlobalVariable* dart_function_pool_ = nullptr;
  llvm::GlobalVariable* runtime_functions_ = nullptr;
  llvm::GlobalVariable* llvm_to_dart_trampoline_ = nullptr;
  llvm::GlobalVariable* temp_dispatch_table_ = nullptr;
  llvm::Function* read_sp_ = nullptr;
  llvm::Function* llvm_to_runtime_trampoline_ = nullptr;
  llvm::Function* create_array_stub_ = nullptr;
  llvm::Function* write_barrier_ = nullptr;
  llvm::Function* no_such_method_ = nullptr;
  llvm::Function* new_object_ = nullptr;


  static const std::unordered_map<RuntimeEntryTag, std::string>
      kRuntimeEntryTagToName;
};

class InstructionInputExtractor {
 public:
  InstructionInputExtractor(const ILInstruction*);

  std::string NextInput();
  intptr_t NextInputAsIntPtr();
  intptr_t NextInputAsInt64();
  template <typename T>
  T NextInputAsEnum();
  size_t NumInputs() const;
  size_t NumInputsLeft() const;
  const ILInstruction* instr() const { return instr_; }

 private:
  const ILInstruction* instr_ = nullptr;
  size_t idx_ = 0;
};

class CodegenHelper : public CodegenTypeCache, public DartVMConstants {
 public:
  explicit CodegenHelper(CodegenModule& cgm, llvm::Function* function);

  llvm::Constant* GetConstantInt(int64_t val, llvm::IntegerType* ty = nullptr) const;

  void SetInvariantLoad(llvm::LoadInst* val);

  static llvm::PointerType* GetNonGCPointer(llvm::Type* ty);

  llvm::Value* EmitBool(llvm::Value* val);
  llvm::Value* EmitFalse();
  llvm::Value* EmitTrue();
  llvm::Value* EmitNull();

  llvm::Value* EmitTestBit(llvm::Value* val, int bit_num);

  llvm::Value* EmitIsNewObject(llvm::Value* val);

  void EmitBrSlowFast(llvm::Value* cond,
                      llvm::BasicBlock* slow_path,
                      llvm::BasicBlock* fast_path);
  void EmitBrFastSlow(llvm::Value* cond,
                      llvm::BasicBlock* fast_path,
                      llvm::BasicBlock* slow_path);

  llvm::Value* EmitNegateBool(llvm::Value* val);

  llvm::Value* EmitIsAnyNotSmi(llvm::Value* a, llvm::Value* b);
  llvm::Value* EmitIsNotSmi(llvm::Value* val);
  llvm::Value* EmitSmiToInt(llvm::Value* val);

  llvm::Value* EmitUnsafePtrToInt(llvm::Value* val);
  llvm::Value* EmitSmiTag(llvm::Value* val);
  llvm::Value* EmitUntagSmi(llvm::Value* val);
  llvm::Value* EmitUntagObject(llvm::Value* obj);
  llvm::Value* EmitTagObject(llvm::Value* obj);

  llvm::Value* EmitFieldPointer(llvm::Value* obj,
                                intptr_t offset,
                                llvm::Type* ty);

  llvm::Value* EmitFieldPointer(llvm::Value* obj,
                                llvm::Value* offset,
                                llvm::Type* ty);

  llvm::Value* EmitNewObject(llvm::Value* ptr);
  llvm::Value* EmitNewObjectAndTag(llvm::Value* ptr);

  llvm::Value* EmitBoxAllocation(size_t cid);

  llvm::Value* EmitStackArray(intptr_t size);
  void EmitStoreInArray(llvm::Value* arr, intptr_t i, llvm::Value* val);
  llvm::Value* EmitStackArrayToPtr(llvm::Value* arr);

  llvm::Value* EmitNativeArguments(llvm::Value* ret_val,
                                   std::vector<llvm::Value*> args);

  void EmitCallToRuntimeTrampoline(CodegenModule::RuntimeEntryTag tag,
                                   llvm::Value* native_args);

  llvm::Value* EmitLoadConstant(intptr_t const_id);


  llvm::Value* EmitLoadFieldFromGlobal(llvm::Value* global,
                                       llvm::Value* offset,
                                       llvm::Type* type,
                                       bool invariant);
  llvm::Value* EmitLoadFieldFromGlobal(llvm::Value* global,
                                       size_t offset,
                                       llvm::Type* type,
                                       bool invariant);

  llvm::Value* EmitLoadField(llvm::Value* object,
                             intptr_t offset,
                             llvm::Type* type);
  llvm::Value* EmitLoadField(llvm::Value* object,
                             llvm::Value* offset,
                             llvm::Type* type);

  void EmitStoreField(llvm::Value* object, intptr_t offset, llvm::Value* val);
  void EmitStoreField(llvm::Value* object,
                      llvm::Value* offset,
                      llvm::Value* val);

  llvm::Value* EmitLoadFieldRaw(llvm::Value* object,
                                intptr_t offset,
                                llvm::Type* type);
  llvm::Value* EmitLoadFieldRaw(llvm::Value* object,
                                llvm::Value* offset,
                                llvm::Type* type);

  void EmitStoreFieldRaw(llvm::Value* object,
                         intptr_t offset,
                         llvm::Value* val);
  void EmitStoreFieldRaw(llvm::Value* object,
                         llvm::Value* offset,
                         llvm::Value* val);

  void EmitStoreInObject(llvm::Value* object,
                         intptr_t offset,
                         llvm::Value* val,
                         bool can_value_be_smi);

  void EmitStoreInObject(llvm::Value* object,
                         llvm::Value* offset,
                         llvm::Value* val,
                         bool can_value_be_smi);

  llvm::Value* GetParam(size_t i) const;
  llvm::Value* GetThread() const;

  llvm::BasicBlock* CreateBasicBlock(intptr_t id);
  llvm::BasicBlock* CreateBasicBlock(std::string id);

  llvm::IRBuilder<llvm::ConstantFolder, llvm::IRBuilderDefaultInserter>&
  builder() const {
    return *builder_;
  }

 private:
  llvm::Value* EmitNativeArguments(intptr_t argc_tag,
                                   llvm::Value* argv,
                                   llvm::Value* ret_val);

  CodegenModule& cgm_;
  llvm::Function* function_ = nullptr;

  std::unique_ptr<
      llvm::IRBuilder<llvm::ConstantFolder, llvm::IRBuilderDefaultInserter>>
      builder_;
};

class CodegenFunction : public CodegenTypeCache, public DartVMConstants {
 public:
  explicit CodegenFunction(CodegenModule& cgm,
                           DispatchTable* dispatch_table,
                           const DartFunction* func,
                           llvm::Function* llvm_func);

  void GenerateCode();
  void PatchDynamicCalls();

  static const int kThreadOffset = 0;
  // The offset of Dart function parameters in LLVM function signatures.
  static const int kDartParamOffset = 1;

 private:
  void GenerateBasicBlock(const DartBasicBlock* bb, llvm::BasicBlock* llvm_bb);

  void EmitInstruction(const ILInstruction* instr);

#define DECLARE_EMIT(type, attrs)                                              \
  llvm::Value* Emit##type(InstructionInputExtractor I);
  FOR_EACH_SUPPORTED_INSTRUCTION(DECLARE_EMIT)
#undef DECLARE_EMIT
  llvm::Value* EmitComparisonOpInt64(TokenKind op,
                                     llvm::Value* left,
                                     llvm::Value* right);

  llvm::Value* EmitComparisonOpDouble(TokenKind op,
                                      llvm::Value* left,
                                      llvm::Value* right);

  llvm::Value* EmitIntegerArithmetic(TokenKind op,
                                     llvm::Value* left,
                                     llvm::Value* right);

  llvm::Constant* GetConstantInt(int64_t val, llvm::IntegerType* ty = nullptr) const;

  void EmitNullError();
  void EmitRangeError(llvm::Value* index, llvm::Value* length);
  void EmitNonBoolTypeError(llvm::Value* val);

  llvm::Value* EmitLLVMStaticCall(size_t llvm_function_id,
                                  const Selector& selector,
                                  const std::vector<std::string>& args);
  llvm::Value* EmitDartStaticCall(intptr_t dart_function_id,
                                  const Selector& selector,
                                  const std::vector<std::string>& args);

  llvm::Value* EmitCid(llvm::Value* obj, bool check_smi);

  llvm::Type* GetTypeFromRepresentation(Representation representation) const;
  llvm::Type* GetUnboxedTypeForCid(int cid) const;
  intptr_t GetValueOffsetForCid(int cid) const;

  void ProcessPhiIncomingValues();

  void AddValue(std::string value_id, llvm::Value* value);
  llvm::Value* GetValue(const std::string& value_id) const;
  llvm::Value* GetValueOrConstant(const std::string& value_id) const;
  llvm::BasicBlock* GetBlock(intptr_t block_id) const;
  llvm::BasicBlock* GetFinalBlock(intptr_t block_id) const;

  bool is_generated_ = false;
  bool is_patched_ = false;
  CodegenModule& cgm_;
  DispatchTable* dispatch_table_ = nullptr;
  const DartFunction* func_ = nullptr;

  std::unordered_map<std::string, llvm::Value*> values_;
  std::unordered_map<intptr_t, llvm::BasicBlock*> blocks_;
  std::unordered_map<intptr_t, llvm::BasicBlock*> final_blocks_;
  std::vector<const ILInstruction*> phi_instructions_;
  std::vector<std::pair<std::string, llvm::Value*>> dynamic_calls_to_patch_;

  CodegenHelper helper_;
  llvm::IRBuilder<llvm::ConstantFolder, llvm::IRBuilderDefaultInserter>&
      builder_;
};

}  // namespace dart_llvm

#endif  // LLVM_CODEGEN_H_
