// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.
#include "llvm_codegen.h"

#include <algorithm>
#include <cassert>
#include <iostream>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include "llvm/Pass.h"
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/Transforms/Vectorize.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>

#include "dispatch_table.h"
#include "il_deserializer.h"

namespace dart_llvm {

namespace {
std::string MangleRuntimeName(const char* entry) {
  return std::string("_ZN4dart") + std::to_string(strlen(entry) + 4) +
         std::string("DRT_") + std::string(entry) +
         std::string("ENS_15NativeArgumentsE");
}
}  // namespace

#define DECLARE_RUNTIME_ENTRY_TAG_TO_NAME_ENTRY(entry)                         \
  {RuntimeEntryTag::k##entry, MangleRuntimeName(#entry)},

const std::unordered_map<CodegenModule::RuntimeEntryTag, std::string>
    CodegenModule::kRuntimeEntryTagToName = {
        LLVM_RUNTIME_ENTRY_LIST(DECLARE_RUNTIME_ENTRY_TAG_TO_NAME_ENTRY)
        {RuntimeEntryTag::kNumEntries, "NumEntries"}};

#undef DECLARE_RUNTIME_ENTRY_TAG_TO_NAME_ENTRY

// TODO(sarkin): Int64Ty needs to be replaced with IntTy in some places.
CodegenModule::CodegenModule(llvm::Module& module, const DartProgram* program)
    : DartVMConstants(*program),
      module_(module),
      llvm_context_(module.getContext()),
      program_(program) {
  dispatch_table_ =
      std::make_unique<DispatchTable>(*this, program->method_table());

  // Use non-integral pointers for GC-able address space
  // which inhibits incorrect optimizations.
  module_.setDataLayout("S128-ni:1");

  // TODO(sarkin): Set correct target.
#ifdef TARGET_X64
  module_.setTargetTriple("x86_64-unknown-linux-gnu");
#endif

#ifdef TARGET_WASM
  module_.setTargetTriple("wasm32-unknown-unknown");
#endif

  // Used when converting LLVM's Int1Ty to Dart bools by adding
  // the Int1 to the False object's offset in the constant pool.
  assert(program->true_offset() == program->false_offset() + 1);

  Int1Ty = llvm::IntegerType::getInt1Ty(llvm_context_);
  Int8Ty = llvm::IntegerType::getInt8Ty(llvm_context_);
  Int16Ty = llvm::IntegerType::getInt16Ty(llvm_context_);
  Int32Ty = llvm::IntegerType::getInt32Ty(llvm_context_);
  Int64Ty = llvm::IntegerType::getInt64Ty(llvm_context_);

  // TODO(sarkin): Platform dependent.
  IntTy = Int64Ty;

  FloatTy = llvm::Type::getFloatTy(llvm_context_);
  DoubleTy = llvm::Type::getDoubleTy(llvm_context_);
  VoidTy = llvm::Type::getVoidTy(llvm_context_);

  Int32x4Ty = llvm::VectorType::get(Int32Ty, 4);

  Float32x4Ty = llvm::VectorType::get(FloatTy, 4);
  Float64x2Ty = llvm::VectorType::get(DoubleTy, 2);

  ObjectPtrTy = llvm::PointerType::get(Int8Ty, kGCAddressSpace);
  PtrToPtrTy = llvm::PointerType::get(ObjectPtrTy, kGCAddressSpace);
  NonGCObjectPtrTy =
      llvm::PointerType::getInt8PtrTy(llvm_context_, kNonGCAddressSpace);
  NonGCPtrToPtrTy = llvm::PointerType::get(ObjectPtrTy, kNonGCAddressSpace);

  ThreadObjectTy = NonGCObjectPtrTy;
  FunctionIDTy = Int64Ty;
  StackArrayTy = NonGCPtrToPtrTy;

  ConstantPoolTy = llvm::ArrayType::get(ObjectPtrTy, program->num_constants());
  constant_pool_ = new llvm::GlobalVariable(
      module_, ConstantPoolTy, false, llvm::GlobalVariable::ExternalLinkage,
      nullptr, kGlobalConstantPoolName);

  FunctionPoolTy =
      llvm::ArrayType::get(NonGCObjectPtrTy, program->NumFunctions());
  function_pool_ = new llvm::GlobalVariable(
      module_, FunctionPoolTy, false, llvm::GlobalVariable::ExternalLinkage,
      nullptr, kGlobalFunctionPoolName);

  DartFunctionPoolTy =
      llvm::ArrayType::get(ObjectPtrTy, program->num_dart_functions());
  dart_function_pool_ = new llvm::GlobalVariable(
      module_, DartFunctionPoolTy, false, llvm::GlobalVariable::ExternalLinkage,
      nullptr, kGlobalDartFunctionPoolName);

  {
    auto target_code_ty = ObjectPtrTy;
    auto arg_descriptor_ty = ObjectPtrTy;
    auto num_args_ty = Int64Ty;
    LLVMToDartTrampolineTy = llvm::FunctionType::get(
        ObjectPtrTy,
        {ThreadObjectTy, target_code_ty, arg_descriptor_ty, num_args_ty,
         StackArrayTy},
        false);
    llvm_to_dart_trampoline_ = new llvm::GlobalVariable(
        module_, NonGCObjectPtrTy, false, llvm::GlobalVariable::ExternalLinkage,
        nullptr, kGlobalLLVMToDartTrampolineName);
  }

  FixedParamTrampolineTy = llvm::FunctionType::get(
      ObjectPtrTy, {FunctionIDTy, ThreadObjectTy, StackArrayTy}, false);

  StaticTrampolinesTy =
      llvm::ArrayType::get(NonGCObjectPtrTy, program->NumFunctions());
  static_trampolines_ = new llvm::GlobalVariable(
      module_, StaticTrampolinesTy, false,
      llvm::GlobalVariable::InternalLinkage, nullptr, "_StaticTrampolines");

  NativeArgumentsTy = llvm::StructType::create(
      llvm_context_,
      {ThreadObjectTy, /* argc */ Int64Ty, StackArrayTy, StackArrayTy},
      "NativeArguments");
  // Non GC-able because always allocated on the stack.
  NativeArgumentsPtrTy =
      llvm::PointerType::get(NativeArgumentsTy, kNonGCAddressSpace);

  RuntimeFunctionTy = llvm::FunctionType::get(/* Return type */ VoidTy,
                                              {NativeArgumentsPtrTy}, false);

  RuntimeFunctionsTy =
      llvm::ArrayType::get(NonGCObjectPtrTy, NumRuntimeFunctions());
  runtime_functions_ = new llvm::GlobalVariable(
      module_, RuntimeFunctionsTy, false, llvm::GlobalVariable::InternalLinkage,
      nullptr, "_RuntimeFunctions");

  DispatchTableEntryTy = llvm::StructType::create(
      llvm_context_, {Int64Ty, NonGCObjectPtrTy}, "DispatchTableEntry");
  DispatchTableTy =
      llvm::PointerType::get(DispatchTableEntryTy, kNonGCAddressSpace);

  // Used temporarily when generating instance calls then replaced with the "real"
  // dispatch table.
  temp_dispatch_table_ = new llvm::GlobalVariable(
      GetModule(), DispatchTableTy, false, llvm::GlobalVariable::ExternalLinkage,
      nullptr, "_TempDispatchTable");
}

const llvm::DataLayout& CodegenModule::GetDataLayout() const {
  return module_.getDataLayout();
}

llvm::Function* CodegenModule::GetFunctionByID(size_t id) const {
  auto it = function_by_id_.find(id);
  assert(it != function_by_id_.end());
  return it->second;
}

const DartFunction* CodegenModule::GetDartFunctionByID(size_t id) const {
  return program_->FunctionAt(id);
}

size_t CodegenModule::GetFunctionIDByPatchPoint(
    const std::string& patch_point) const {
  assert(!patch_point.empty() && patch_point[0] == '1');
  return program_->GetFunctionIDForPatchPoint(patch_point);
}

llvm::Function* CodegenModule::GetFunctionByPatchPoint(
    const std::string& patch_point) const {
  assert(!patch_point.empty() && patch_point[0] == '1');
  return GetFunctionByID(GetFunctionIDByPatchPoint(patch_point));
}

size_t CodegenModule::GetDartFunctionIndexByPatchPoint(
    const std::string& patch_point) const {
  assert(!patch_point.empty() && patch_point[0] == '0');
  return program_->GetFunctionIDForPatchPoint(patch_point);
}

llvm::Function* CodegenModule::GetOrCreateHandleOptionalParamsTrampoline(
    size_t function_id,
    const Selector& selector) {
  auto function = program_->FunctionAt(function_id);
  std::string trampoline_name = selector.selector();

  auto it = optional_param_trampoline_cache_.find(
      make_pair(function_id, trampoline_name));
  if (it != optional_param_trampoline_cache_.end()) {
    return it->second;
  }

  const auto argc = selector.argc() + CodegenFunction::kDartParamOffset;

  std::vector<llvm::Type*> params(argc, ObjectPtrTy);
  params[0] = ThreadObjectTy;
  auto trampoline_ty = llvm::FunctionType::get(ObjectPtrTy, params, false);
  auto trampoline = llvm::Function::Create(
      trampoline_ty, llvm::GlobalVariable::InternalLinkage, trampoline_name,
      module_);
  auto target_fn = GetDartFunctionByID(function_id);
  // Set GC strategy only if the target function can trigger GC, because
  // the trampoline only forwards the arguments and does nothing that can
  // trigger GC by itself.
  if (target_fn->can_trigger_gc()) {
    trampoline->setGC(kGCStrategyName);
  }

  CodegenHelper helper(*this, trampoline);
  auto bb = helper.CreateBasicBlock("entry");
  auto& builder = helper.builder();
  builder.SetInsertPoint(bb);

  const auto num_params =
      function->num_params() + CodegenFunction::kDartParamOffset;
  const auto num_fixed_params =
      function->NumFixedParameters() + CodegenFunction::kDartParamOffset;
  const auto num_positional_args =
      selector.NumPositionalArgs() + CodegenFunction::kDartParamOffset;

  std::vector<llvm::Value*> forward_args(num_params);
  for (size_t i = 0; i < num_positional_args; ++i) {
    forward_args[i] = helper.GetParam(i);
  }

  if (!function->HasNamedParameters()) {
    for (size_t i = num_positional_args; i < num_params; ++i) {
      auto const_id = function->DefaultParamValueAt(i - num_fixed_params);
      forward_args[i] = helper.EmitLoadConstant(const_id);
    }
  } else {
    const auto& named_args = selector.named_args();
    auto get_named_arg_index = [&](const std::string& name) -> intptr_t {
      auto it = std::find(named_args.begin(), named_args.end(), name);
      return (it == named_args.end())
                 ? -1
                 : it - named_args.begin() + num_fixed_params;
    };
    for (size_t i = 0; i < function->NumOptionalParameters(); ++i) {
      auto named_arg_i = get_named_arg_index(function->NamedParameterAt(i));
      auto forward_arg_i = i + num_fixed_params;
      if (named_arg_i < 0) {
        auto const_id = function->DefaultParamValueAt(i);
        forward_args[forward_arg_i] = helper.EmitLoadConstant(const_id);
      } else {
        forward_args[forward_arg_i] = helper.GetParam(named_arg_i);
      }
    }
  }
  auto call = builder.CreateCall(GetFunctionByID(function_id), {forward_args});
  call->setTailCallKind(llvm::CallInst::TailCallKind::TCK_Tail);
  
  builder.CreateRet(call);

  optional_param_trampoline_cache_.emplace(
      std::make_pair(function_id, trampoline_name), trampoline);

  return trampoline;
}

llvm::Function* CodegenModule::GetOrCreateDartCallTrampoline(
    size_t function_id,
    const Selector& selector) {
  std::string trampoline_name = selector.selector();
  auto it =
      dart_call_trampoline_cache_.find(make_pair(function_id, trampoline_name));
  if (it != dart_call_trampoline_cache_.end()) {
    return it->second;
  }

  auto argc = selector.argc() + CodegenFunction::kDartParamOffset;
  std::vector<llvm::Type*> params(argc, ObjectPtrTy);
  params[0] = ThreadObjectTy;
  auto trampoline_ty = llvm::FunctionType::get(ObjectPtrTy, params, false);
  auto trampoline = llvm::Function::Create(
      trampoline_ty, llvm::GlobalVariable::InternalLinkage, trampoline_name,
      module_);
  trampoline->setGC(kGCStrategyName);

  CodegenHelper helper(*this, trampoline);
  auto bb = helper.CreateBasicBlock("entry");
  auto& builder = helper.builder();
  builder.SetInsertPoint(bb);

  assert(selector.arg_descriptor_id() >= 0);
  auto arg_descriptor = helper.EmitLoadConstant(selector.arg_descriptor_id());
  auto arg_array = helper.EmitStackArray(selector.argc());
  for (size_t i = 0; i < selector.argc(); ++i) {
    helper.EmitStoreInArray(
        arg_array, i, helper.GetParam(i + CodegenFunction::kDartParamOffset));
  }
  auto arg_array_ptr = helper.EmitStackArrayToPtr(arg_array);
  auto code_object = helper.EmitLoadFieldFromGlobal(
      dart_function_pool_, function_id, ObjectPtrTy, true);

  std::vector<llvm::Value*> trampoline_args{
      helper.GetThread(), code_object, arg_descriptor,
      GetConstantInt(selector.argc()), arg_array_ptr};
  auto callee_ty = helper.GetNonGCPointer(LLVMToDartTrampolineTy);
  llvm::Value* callee = builder.CreateLoad(llvm_to_dart_trampoline_);
  callee = builder.CreateBitCast(callee, callee_ty);
  builder.CreateRet(builder.CreateCall(callee, trampoline_args));

  dart_call_trampoline_cache_.emplace(
      std::make_pair(function_id, trampoline_name), trampoline);

  return trampoline;
}

llvm::Function* CodegenModule::GetOrCreateDynamicTrampoline(
    const DartFunctionDeclaration* target,
    const Selector& selector) {
  if (target->is_llvm()) {
    return GetOrCreateHandleOptionalParamsTrampoline(target->id(), selector);
  }
  return GetOrCreateDartCallTrampoline(target->id(), selector);
}

llvm::Function* CodegenModule::GetOrCreateClassAllocationStub(size_t cid) {
  auto it = class_allocation_stub_cache_.find(cid);
  if (it != class_allocation_stub_cache_.end()) {
    return it->second;
  }

  auto allocation_info = GetClassAllocationInfo(cid);
  assert(allocation_info != nullptr);

  // (Thread object, Class object, Type arguments)
  const int kThreadObjectOffset = 0;
  const int kClassObjectOffset = 1;
  const int kTypeArgumentsOffset = 2;
  std::vector<llvm::Type*> params{ThreadObjectTy, ObjectPtrTy, ObjectPtrTy};
  auto stub_ty = llvm::FunctionType::get(ObjectPtrTy, params, false);
  auto stub_name = "_LLVMAllocationStub_" + std::to_string(cid);
  auto stub = llvm::Function::Create(
      stub_ty, llvm::GlobalVariable::InternalLinkage, stub_name, module_);
  stub->setGC(kGCStrategyName);

  CodegenHelper helper(*this, stub);
  auto bb = helper.CreateBasicBlock("entry");
  auto& builder = helper.builder();
  builder.SetInsertPoint(bb);

  auto class_object = helper.GetParam(kClassObjectOffset);
  auto type_arguments = helper.GetParam(kTypeArgumentsOffset);

  auto slow_path = helper.CreateBasicBlock("slow_path");
  if (allocation_info->has_fast_path()) {
    auto fast_path = helper.CreateBasicBlock("fast_path");
    builder.CreateBr(fast_path);
    builder.SetInsertPoint(fast_path);

    auto thread_object = helper.GetParam(kThreadObjectOffset);
    auto fast_path_cont = helper.CreateBasicBlock("fast_path_cont");
    auto top =
        helper.EmitLoadFieldRaw(thread_object, kThreadTopOffset, Int64Ty);
    auto size = GetConstantInt(allocation_info->instance_size());
    auto obj_end = builder.CreateAdd(top, size);
    {
      auto end =
          helper.EmitLoadFieldRaw(thread_object, kThreadEndOffset, Int64Ty);
      auto cmp = builder.CreateICmpSGE(obj_end, end);
      helper.EmitBrSlowFast(cmp, slow_path, fast_path_cont);
    }
    builder.SetInsertPoint(fast_path_cont);

    helper.EmitStoreFieldRaw(thread_object, kThreadTopOffset, obj_end);

    auto new_obj = helper.EmitNewObjectAndTag(top);
    // 64-bit operation is intended to zero out higher bits.
    helper.EmitStoreField(new_obj, kInstanceTagsOffset,
                          GetConstantInt(allocation_info->tags()));

    {
      auto null = helper.EmitNull();
      llvm::Value* initial = GetConstantInt(kInstanceNextFieldOffset);

      auto loop_head = helper.CreateBasicBlock("init_head");
      auto loop_body = helper.CreateBasicBlock("init_body");
      auto loop_done = helper.CreateBasicBlock("init_done");

      auto entry = builder.GetInsertBlock();
      builder.CreateBr(loop_head);

      builder.SetInsertPoint(loop_head);
      auto counter = builder.CreatePHI(initial->getType(), 2);
      counter->addIncoming(initial, entry);

      auto cmp = builder.CreateICmpSGE(counter, size);
      builder.CreateCondBr(cmp, loop_done, loop_body);

      builder.SetInsertPoint(loop_body);
      helper.EmitStoreField(new_obj, counter, null);
      auto next_counter = builder.CreateAdd(counter, GetConstantInt(kWordSize));
      counter->addIncoming(next_counter, loop_body);
      builder.CreateBr(loop_head);

      builder.SetInsertPoint(loop_done);
    }
    if (allocation_info->is_parameterized()) {
      helper.EmitStoreField(new_obj,
                            allocation_info->type_arguments_field_offset(),
                            type_arguments);
    }
    builder.CreateRet(new_obj);
  } else {
    builder.CreateBr(slow_path);
  }

  builder.SetInsertPoint(slow_path);

  // (Class object, Type arguments)
  auto object = builder.CreateAlloca(ObjectPtrTy);
  auto native_args =
      helper.EmitNativeArguments(object, {class_object, type_arguments});
  helper.EmitCallToRuntimeTrampoline(RuntimeEntryTag::kAllocateObject,
                                     native_args);

  builder.CreateRet(builder.CreateLoad(object));

  class_allocation_stub_cache_.emplace(cid, stub);
  return stub;
}

// Statepoint IDs are used when traversing the stack to identify the statepoint.
struct AddStatepointIDsToCallSites : public llvm::FunctionPass {
  static char ID;  // Pass identification, replacement for typeid.
  CodegenModule& cgm;
  int64_t id = 0;

  explicit AddStatepointIDsToCallSites(CodegenModule& cgm)
      : FunctionPass(ID), cgm(cgm) {}

  void addStatepoints(llvm::BasicBlock& bb, llvm::Function* func) {
    for (llvm::Instruction& instruction : bb) {
      if (auto call = llvm::dyn_cast<llvm::CallInst>(&instruction)) {
        cgm.AddStatepoint(func);
        auto attr = llvm::Attribute::get(cgm.GetLLVMContext(), "statepoint-id",
                                         std::to_string(id));
        id++;
        call->addAttribute(llvm::AttributeList::FunctionIndex, attr);
      }
    }
  }

  bool runOnFunction(llvm::Function& func) override {
    for (llvm::BasicBlock& bb : func) {
      addStatepoints(bb, &func);
    }
    return false;
  }
};

char AddStatepointIDsToCallSites::ID = 0;

// The dart_new_object intrinsic is required when creating new GC tracked
// pointers, e.g. in the fast path of allocation, because it's illegal
// to have IntToPtr to the GC tracked address space, and the
// RewriteStatepointsForGC pass will complain if it encounters such
// casts. The intrinsic "hides" the address space cast and is lowered
// only after the RewriteStatepointsForGC pass.
struct RewriteGCIntrinsics : public llvm::FunctionPass {
  static char ID;  // Pass identification, replacement for typeid.
  CodegenModule& cgm_;

  RewriteGCIntrinsics(CodegenModule& cgm) : FunctionPass(ID), cgm_(cgm) {}

  bool tryRewrite(llvm::BasicBlock& bb) {
    for (llvm::Instruction& instruction : bb) {
      if (llvm::CallInst* call = llvm::dyn_cast<llvm::CallInst>(&instruction)) {
        llvm::Function* fn = call->getCalledFunction();
        if (fn && fn->isIntrinsic()) {
          fn->recalculateIntrinsicID();
          llvm::IRBuilder<> b(&instruction);
          // Have to stop iteration by returning true if block structure has
          // changed.
          switch (fn->getIntrinsicID()) {
            case llvm::Intrinsic::dart_new_object: {
              auto pointer = call->getArgOperand(0);
              pointer = b.CreateAddrSpaceCast(pointer, cgm_.ObjectPtrTy);
              call->replaceAllUsesWith(pointer);
              call->eraseFromParent();
              return true;
            }
            default:
              break;
          }
        }
      }
    }
    return false;
  }

  bool runOnFunction(llvm::Function& func) override {
    for (llvm::BasicBlock& bb : func) {
      while (tryRewrite(bb)) {
      }
    }
    return false;
  }
};

char RewriteGCIntrinsics::ID = 1;

void CodegenModule::OptimizeModule() {
  // TODO(sarkin): 
  auto run_standard_opt = [&](int opt_level, int size_level) {
    llvm::legacy::PassManager mpm;
    llvm::legacy::FunctionPassManager fpm(&module_);
    llvm::PassManagerBuilder builder;
    builder.OptLevel = opt_level;
    builder.SizeLevel = size_level;

    builder.populateFunctionPassManager(fpm);
    builder.populateModulePassManager(mpm);

    for (auto& f : module_) {
      fpm.run(f);
    }
    mpm.run(module_);
  };

  auto optimize_before = [&]() {
    {
      llvm::legacy::PassManager mpm;
      mpm.add(llvm::createFunctionInliningPass());
      mpm.add(llvm::createGlobalDCEPass());

      llvm::legacy::FunctionPassManager fpm(&module_);
      fpm.add(llvm::createPromoteMemoryToRegisterPass());

      fpm.add(llvm::createCFGSimplificationPass());
      fpm.add(llvm::createConstantPropagationPass());
      fpm.add(llvm::createInstructionCombiningPass());
      fpm.add(llvm::createTailCallEliminationPass());
      fpm.add(llvm::createSpeculativeExecutionPass());
      fpm.add(llvm::createAggressiveDCEPass());

      mpm.run(module_);

      for (auto& f : module_) {
        fpm.run(f);
      }
    }
  };

  optimize_before();

  {
#ifdef TARGET_X64
    llvm::legacy::FunctionPassManager fpm(&module_);
    fpm.add(new AddStatepointIDsToCallSites(*this));
    for (auto& f : module_) {
      fpm.run(f);
    }
    llvm::legacy::PassManager mpm;
    mpm.add(llvm::createRewriteStatepointsForGCLegacyPass());
    mpm.run(module_);
#endif
  }

  {
    llvm::legacy::FunctionPassManager fpm(&module_);
    fpm.add(new RewriteGCIntrinsics(*this));
    for (auto& f : module_) {
      fpm.run(f);
    }
  }

  auto optimize_after = [&]() {
    {
      llvm::legacy::PassManager mpm;
      mpm.add(llvm::createFunctionInliningPass());
      mpm.add(llvm::createGlobalDCEPass());

      llvm::legacy::FunctionPassManager fpm(&module_);
      fpm.add(llvm::createPromoteMemoryToRegisterPass());
      fpm.add(llvm::createCFGSimplificationPass());
      fpm.add(llvm::createConstantPropagationPass());
      fpm.add(llvm::createLICMPass());
      fpm.add(llvm::createGVNPass());
      fpm.add(llvm::createEarlyCSEPass());
      fpm.add(llvm::createInstructionCombiningPass());
      fpm.add(llvm::createTailCallEliminationPass());
      fpm.add(llvm::createSpeculativeExecutionPass());
      fpm.add(llvm::createAggressiveDCEPass());

      mpm.run(module_);

      for (auto& f : module_) {
      fpm.run(f);
      }
    }
  };
  // optimize_after();
  // TODO(sarkin): Sometimes O3 does a worse job than optimize_after.
  run_standard_opt(3, 1);
}

void CodegenModule::GenerateProgram() {
  assert(!is_generated_);
  is_generated_ = true;

  assert(program_ != nullptr);
  // Order matters.
  InitializeGlobals();
  GenerateRuntimeFunctionDeclarations();
  GenerateSpecialFunctions();
  GenerateFunctions();

  OptimizeModule();

  // Done after optimization, since Statepoint IDs should be
  // assigned after inlining.
  CreateStatepointIDToFunctionTable();
}

// Signature: Object* (Thread, Object* args...)
llvm::FunctionType* CodegenModule::GetStaticFunctionType(size_t num_params) {
  std::vector<llvm::Type*> params;
  params.push_back(ThreadObjectTy);
  for (size_t i = 0; i < num_params; ++i) {
    params.push_back(ObjectPtrTy);
  }
  auto return_ty = ObjectPtrTy;
  return llvm::FunctionType::get(return_ty, params, false);
}

llvm::Constant* CodegenModule::GetConstantInt(int64_t val,
                                              llvm::IntegerType* ty) const {
  if (ty == nullptr) {
    ty = IntTy;
  }
  return llvm::ConstantInt::get(ty, val);
}

void CodegenModule::AddStatepoint(llvm::Function* func) {
  statepoint_id_to_func_.push_back(func);
}

void CodegenModule::CreateStatepointIDToFunctionTable() {
  auto size = statepoint_id_to_func_.size();
  auto table_ty = llvm::ArrayType::get(NonGCObjectPtrTy, size);
  std::vector<llvm::Constant*> entries(size);
  for (size_t i = 0; i < size; ++i) {
    entries[i] = llvm::ConstantExpr::getBitCast(statepoint_id_to_func_[i],
                                                NonGCObjectPtrTy);
  }
  auto initializer = llvm::ConstantArray::get(table_ty, entries);
  auto table = new llvm::GlobalVariable(
      GetModule(), table_ty, false, llvm::GlobalVariable::ExternalLinkage,
      initializer, "_LLVMStatepointIDToFunction");
  (void)table;
}

void CodegenModule::GenerateRuntimeFunctionDeclarations() {
  std::vector<llvm::Constant*> runtime_functions(NumRuntimeFunctions());

  for (size_t i = 0; i < NumRuntimeFunctions(); ++i) {
    auto it = kRuntimeEntryTagToName.find(static_cast<RuntimeEntryTag>(i));
    assert(it != kRuntimeEntryTagToName.end());
    auto func = llvm::Function::Create(RuntimeFunctionTy,
                                       llvm::GlobalVariable::ExternalLinkage,
                                       it->second, module_);
    // Pass NativeArguments by value
    func->addParamAttr(0, llvm::Attribute::AttrKind::ByVal);

    runtime_functions[i] =
        llvm::ConstantExpr::getBitCast(func, NonGCObjectPtrTy);
  }
  runtime_functions_->setInitializer(
      llvm::ConstantArray::get(RuntimeFunctionsTy, runtime_functions));
}

void CodegenModule::GenerateSpecialFunctions() {
  // None of these are required for WASM, but generate some of them
  // to bypass linker errors.
#ifndef TARGET_WASM
  GenerateDartToLLVMTrampoline();
  GenerateReadStackPointer();
#endif
  GenerateFixedParamsTrampolines();
  GenerateLLVMToRuntimeTrampoline();
  GenerateCreateArrayStub();
  GenerateWriteBarrier();
#ifndef TARGET_WASM
  GenerateGetStackMaps();
#endif
}

void CodegenModule::GenerateDartToLLVMTrampoline() {
  llvm::Function* trampoline = llvm::Function::Create(
      FixedParamTrampolineTy, llvm::GlobalVariable::ExternalLinkage,
      kDartToLLVMTrampolineName, module_);

  trampoline->setGC(kGCStrategyName);

  CodegenHelper helper(*this, trampoline);
  auto bb = helper.CreateBasicBlock("entry");
  auto& builder = helper.builder();
  builder.SetInsertPoint(bb);

  llvm::Value* func_id = helper.GetParam(0);
  llvm::Value* thr = helper.GetParam(1);;
  llvm::Value* args = helper.GetParam(2);

  auto callee_ty = helper.GetNonGCPointer(FixedParamTrampolineTy);
  auto callee = helper.EmitLoadFieldFromGlobal(static_trampolines_, func_id,
                                               callee_ty, true);

  builder.CreateRet(builder.CreateCall(callee, {func_id, thr, args}));
}

void CodegenModule::GenerateLLVMToRuntimeTrampoline() {
  const int kFunctionIDOffset = 2;
  const int kNativeArgumentsOffset = 3;
  auto trampoline_ty =
      llvm::FunctionType::get(VoidTy,
                              {ThreadObjectTy, /* exit_frame offset */ Int64Ty,
                               FunctionIDTy, NativeArgumentsPtrTy},
                              false);
  auto trampoline = llvm::Function::Create(
      trampoline_ty, llvm::GlobalVariable::InternalLinkage,
      "_LLVMToRuntimeTrampoline", module_);
  // Explicitly set calling convention, as the function is internal and
  // the calling convention might be optimized to fastcc.
  trampoline->setCallingConv(llvm::CallingConv::C);
  // No inling, as inling can break the exit frame setup.
  trampoline->addAttribute(llvm::AttributeList::FunctionIndex,
                           llvm::Attribute::AttrKind::NoInline);
  // Do not need to set GC strategy for this function, as it's the "exit frame"
  // and is never visited during GC.

  CodegenHelper helper(*this, trampoline);
  auto bb = helper.CreateBasicBlock("entry");
  auto& builder = helper.builder();
  builder.SetInsertPoint(bb);

  // Setup top exit frame.
#ifndef TARGET_WASM
  auto sp = builder.CreateCall(read_sp());
  helper.EmitStoreFieldRaw(helper.GetThread(), kThreadTopExitFrameInfoOffset,
                           sp);

  llvm::Value* func_id = helper.GetParam(kFunctionIDOffset);
  llvm::Value* native_args = helper.GetParam(kNativeArgumentsOffset);
  auto callee_ty = helper.GetNonGCPointer(RuntimeFunctionTy);
  auto callee = helper.EmitLoadFieldFromGlobal(runtime_functions_, func_id,
                                               callee_ty, true);
  auto call = builder.CreateCall(callee, {native_args});
  // Pass NativeArguments by value.
  call->addParamAttr(0, llvm::Attribute::ByVal);
#endif
  builder.CreateRetVoid();

  llvm_to_runtime_trampoline_ = trampoline;
}

void CodegenModule::GenerateReadStackPointer() {
  // TODO(sarkin): Currently only works for x64. For other architectures,
  // need to change the name of the register.
#ifdef TARGET_X64
  auto& func = read_sp_;
  auto func_ty = llvm::FunctionType::get(IntTy, false);
  func = llvm::Function::Create(func_ty, llvm::GlobalVariable::InternalLinkage,
                                "", module_);
  func->addAttribute(llvm::AttributeList::FunctionIndex,
                   llvm::Attribute::AttrKind::AlwaysInline);

  CodegenHelper helper(*this, func);
  auto bb = helper.CreateBasicBlock("entry");
  auto& builder = helper.builder();
  builder.SetInsertPoint(bb);

  llvm::MDBuilder md_builder(GetLLVMContext());
  auto rsp_node =
      llvm::MDNode::get(GetLLVMContext(), {md_builder.createString("rsp")});
  auto rsp_meta = llvm::MetadataAsValue::get(GetLLVMContext(), rsp_node);
  auto sp = builder.CreateIntrinsic(llvm::Intrinsic::read_register, {IntTy},
                                    {rsp_meta});
  builder.CreateRet(sp);
#endif
}

void CodegenModule::GenerateFixedParamsTrampolines() {
  std::vector<llvm::Constant*> trampolines(program_->NumFunctions());
  for (size_t i = 0; i < program_->NumFunctions(); ++i) {
    auto t =
        GetOrCreateFixedParamTrampoline(program_->FunctionAt(i)->num_params());
    trampolines[i] = llvm::ConstantExpr::getBitCast(t, NonGCObjectPtrTy);
  }
  static_trampolines_->setInitializer(
      llvm::ConstantArray::get(StaticTrampolinesTy, trampolines));
}

void CodegenModule::GenerateCreateArrayStub() {
  // (Thread object, Length, Element Type)
  const int kThreadObjectOffset = 0;
  const int kLengthOffset = 1;
  const int kElementTypeOffset = 2;
  std::vector<llvm::Type*> params{ThreadObjectTy, ObjectPtrTy, ObjectPtrTy};
  auto stub_ty = llvm::FunctionType::get(ObjectPtrTy, params, false);
  auto stub_name = "_LLVMCreateArrayStub";
  auto& stub = create_array_stub_;
  stub = llvm::Function::Create(
      stub_ty, llvm::GlobalVariable::InternalLinkage, stub_name, module_);
  stub->setGC(kGCStrategyName);

  CodegenHelper helper(*this, stub);
  auto bb = helper.CreateBasicBlock("entry");
  auto& builder = helper.builder();
  builder.SetInsertPoint(bb);

  auto length = helper.GetParam(kLengthOffset);
  auto element_type = helper.GetParam(kElementTypeOffset);

  auto slow_path = helper.CreateBasicBlock("slow_path");
  auto max_len_check = helper.CreateBasicBlock("max_len_check");
  auto bump_pointer_carry_check =
      helper.CreateBasicBlock("bump_pointer_carry_check");
  auto fit_in_new_space_check =
      helper.CreateBasicBlock("fit_in_new_space_check");
  auto fast_path = helper.CreateBasicBlock("fast_path");

  auto is_not_smi = helper.EmitIsNotSmi(length);
  helper.EmitBrSlowFast(is_not_smi, slow_path, max_len_check);

  builder.SetInsertPoint(max_len_check);
  llvm::Value* int_length = helper.EmitSmiToInt(length);
  {
    // Smi tag the constant
    auto bound = GetConstantInt(kArrayMaxNewSpaceElements << 1);
    auto cmp = builder.CreateICmpSGT(int_length, bound);
    helper.EmitBrSlowFast(cmp, slow_path, bump_pointer_carry_check);
  }

  auto allocation_size = int_length;
  builder.SetInsertPoint(bump_pointer_carry_check);
  {
    const intptr_t fixed_size_plus_alignment_padding =
        kRawArraySize + kObjectAlignment - 1;
    // Platform dependent. Shift by log2(kWordSize) - 1
    // since length is Smi tagged.
    auto log2 = [](auto n) {
      assert(n);
      int l = 0;
      while (!(n & 1)) {
        l++;
        n >>= 1;
      }
      return l;
    };
    allocation_size = builder.CreateShl(allocation_size, log2(kWordSize) - 1);
    allocation_size = builder.CreateAdd(
        allocation_size, GetConstantInt(fixed_size_plus_alignment_padding));
    allocation_size = builder.CreateAnd(allocation_size, -kObjectAlignment);
  }
  auto thread_object = helper.GetParam(kThreadObjectOffset);
  auto top =
      helper.EmitLoadFieldRaw(thread_object, kThreadTopOffset, Int64Ty);
  llvm::Value* obj_end = nullptr;
  {
    auto uadd = builder.CreateIntrinsic(llvm::Intrinsic::ID::uadd_with_overflow,
                                        {Int64Ty}, {top, allocation_size});
    obj_end = builder.CreateExtractValue(uadd, {0});
    auto cmp = builder.CreateExtractValue(uadd, {1});
    helper.EmitBrSlowFast(cmp, slow_path, fit_in_new_space_check);
  }

  builder.SetInsertPoint(fit_in_new_space_check);
  {
    auto end =
        helper.EmitLoadFieldRaw(thread_object, kThreadEndOffset, Int64Ty);
    auto cmp = builder.CreateICmpSGE(obj_end, end);
    helper.EmitBrSlowFast(cmp, slow_path, fast_path);
  }

  builder.SetInsertPoint(fast_path);
  {
    helper.EmitStoreFieldRaw(thread_object, kThreadTopOffset, obj_end);

    llvm::Value* tags = nullptr;
    {
      auto overflow_bb = helper.CreateBasicBlock("allocation_size_overflow");
      auto no_overflow_bb =
          helper.CreateBasicBlock("allocation_size_no_overflow");
      auto fast_path_cont = helper.CreateBasicBlock("fast_path");

      auto cmp = builder.CreateICmpSGT(allocation_size,
                                       GetConstantInt(kRawObjectMaxSizeTag));
      builder.CreateCondBr(cmp, overflow_bb, no_overflow_bb);

      builder.SetInsertPoint(overflow_bb);
      auto otag = GetConstantInt(0);
      builder.CreateBr(fast_path_cont);

      builder.SetInsertPoint(no_overflow_bb);
      auto notag = builder.CreateShl(
          allocation_size, kRawObjectSizeTagPos - kObjectAlignmentLog2);
      builder.CreateBr(fast_path_cont);

      builder.SetInsertPoint(fast_path_cont);
      auto merge_tag = builder.CreatePHI(Int64Ty, 2);
      merge_tag->addIncoming(otag, overflow_bb);
      merge_tag->addIncoming(notag, no_overflow_bb);

      tags = builder.CreateOr(merge_tag, kRawArrayTags);
    }

    auto new_obj = helper.EmitNewObjectAndTag(top);
    helper.EmitStoreField(new_obj, kArrayTagsOffset, tags);
    helper.EmitStoreField(new_obj, kArrayTypeArgumentsOffset, element_type);
    helper.EmitStoreField(new_obj, kArrayLengthOffset, length);
    {
        auto null = helper.EmitNull();
        llvm::Value* initial = GetConstantInt(kRawArraySize);

        auto loop_head = helper.CreateBasicBlock("init_head");
        auto loop_body = helper.CreateBasicBlock("init_body");
        auto loop_done = helper.CreateBasicBlock("init_done");

        auto entry = builder.GetInsertBlock();
        builder.CreateBr(loop_head);

        builder.SetInsertPoint(loop_head);
        auto counter = builder.CreatePHI(initial->getType(), 2);
        counter->addIncoming(initial, entry);

        auto cmp = builder.CreateICmpSGE(counter, allocation_size);
        builder.CreateCondBr(cmp, loop_done, loop_body);

        builder.SetInsertPoint(loop_body);
        helper.EmitStoreField(new_obj, counter, null);
        auto next_counter =
            builder.CreateAdd(counter, GetConstantInt(kWordSize));
        counter->addIncoming(next_counter, loop_body);
        builder.CreateBr(loop_head);

        builder.SetInsertPoint(loop_done);
    }

    builder.CreateRet(new_obj);
  }

  builder.SetInsertPoint(slow_path);
  // (Length, Element Type)
  auto array = builder.CreateAlloca(ObjectPtrTy);
  auto native_args = helper.EmitNativeArguments(array, {length, element_type});
  helper.EmitCallToRuntimeTrampoline(RuntimeEntryTag::kAllocateArray,
                                     native_args);

  builder.CreateRet(builder.CreateLoad(array));
}

void CodegenModule::GenerateWriteBarrier() {
  // (Thread object, Object, Value)
  const int kObjectOffset = 1;
  const int kValueOffset = 2;
  std::vector<llvm::Type*> params{ThreadObjectTy, ObjectPtrTy, ObjectPtrTy};
  auto barrier_ty = llvm::FunctionType::get(VoidTy, params, false);
  auto barrier_name = "_LLVMWriteBarrier";
  auto& wb = write_barrier_;
  wb = llvm::Function::Create(barrier_ty, llvm::GlobalVariable::InternalLinkage,
                              barrier_name, module_);
  wb->setGC(kGCStrategyName);
  wb->addAttribute(llvm::AttributeList::FunctionIndex,
                   llvm::Attribute::AttrKind::NoInline);

  CodegenHelper helper(*this, wb);
  auto bb = helper.CreateBasicBlock("entry");
  auto& builder = helper.builder();
  builder.SetInsertPoint(bb);

#ifdef TARGET_WASM
  // TODO(sarkin): Hack:
  // For WASM we don't need a write barrier, but we still generate the
  // definition to bypass linker errors.
  builder.CreateRetVoid();
  return;
#endif

  auto thread = helper.GetThread();
  auto object = helper.GetParam(kObjectOffset);
  auto value = helper.GetParam(kValueOffset);
  auto untagged_object = helper.EmitUntagObject(object);
  auto untagged_value = helper.EmitUntagObject(value);

  // Assume concurrent marking.
  auto is_val_new = helper.EmitIsNewObject(value);
  auto add_to_mark_stack = helper.CreateBasicBlock("add_to_mark_stack");
  auto val_is_new = helper.CreateBasicBlock("value_is_new");
  helper.EmitBrFastSlow(is_val_new, val_is_new, add_to_mark_stack);

  builder.SetInsertPoint(val_is_new);
  {
    auto tags =
        helper.EmitFieldPointer(untagged_object, kObjectTagsOffset, Int32Ty);
    // TODO(sarkin): Might be possible to use weaker ordering.
    builder.CreateAtomicRMW(
        llvm::AtomicRMWInst::BinOp::And, tags,
        GetConstantInt(~(1 << kRawObjectOldAndNotRememberedBit), Int32Ty),
        llvm::AtomicOrdering::SequentiallyConsistent);
  }

  // Load the StoreBuffer block out of the thread. Then load top_ out of the
  // StoreBufferBlock and add the address to the pointers_.
  auto store_buffer_block = helper.EmitLoadFieldRaw(
      thread, kThreadStoreBufferBlockOffset, NonGCObjectPtrTy);
  auto store_buffer_top = helper.EmitLoadFieldRaw(
      store_buffer_block, kStoreBufferBlockTopOffset, Int32Ty);

  {
    auto offset = builder.CreateZExt(store_buffer_top, Int64Ty);
    offset = builder.CreateShl(offset, kWordSizeLog2);
    offset = builder.CreateAdd(offset,
                               GetConstantInt(kStoreBufferBlockPointersOffset));
    helper.EmitStoreFieldRaw(store_buffer_block, offset, object);
  }

  auto store_buffer_overflow = helper.CreateBasicBlock("store_buffer_overflow");
  auto store_buffer_no_overflow =
      helper.CreateBasicBlock("store_buffer_no_overflow");

  {
    auto inc_top =
        builder.CreateAdd(store_buffer_top, GetConstantInt(1, Int32Ty));
    inc_top = builder.CreateTrunc(inc_top, Int32Ty);
    helper.EmitStoreFieldRaw(store_buffer_block, kStoreBufferBlockTopOffset,
                             inc_top);
    auto cmp = builder.CreateICmpEQ(
        inc_top, GetConstantInt(kStoreBufferBlockSize, Int32Ty));
    helper.EmitBrSlowFast(cmp, store_buffer_overflow, store_buffer_no_overflow);
  }

  builder.SetInsertPoint(store_buffer_no_overflow);
  builder.CreateRetVoid();

  builder.SetInsertPoint(store_buffer_overflow);
  // Call to runtime.
  {
    auto func_ty = llvm::FunctionType::get(VoidTy, {NonGCObjectPtrTy}, false);
    auto func = llvm::Function::Create(
        func_ty, llvm::GlobalVariable::LinkageTypes::ExternalLinkage,
        "StoreBufferBlockProcess", module_);
    builder.CreateCall(func, {thread});
  }
  builder.CreateRetVoid();

  auto already_marked = helper.CreateBasicBlock("already_marked");
  auto try_mark = helper.CreateBasicBlock("try_mark");
  auto mark_success = helper.CreateBasicBlock("mark_success");

  builder.SetInsertPoint(already_marked);
  builder.CreateRetVoid();

  builder.SetInsertPoint(add_to_mark_stack);
  auto old_tags = helper.EmitLoadField(value, kObjectTagsOffset, Int32Ty);
  {
    auto cmp = helper.EmitTestBit(old_tags, kRawObjectOldAndNotMarkedBit);
    builder.CreateCondBr(cmp, already_marked, try_mark);
  }

  builder.SetInsertPoint(try_mark);
  {
    auto tags =
        builder.CreateAnd(old_tags, ~(1 << kRawObjectOldAndNotMarkedBit));
    auto tags_ptr =
        helper.EmitFieldPointer(untagged_value, kObjectTagsOffset, Int32Ty);
    // TODO(sarkin): Might be possible to use weaker ordering.
    auto xchg = builder.CreateAtomicCmpXchg(
        tags_ptr, old_tags, tags, llvm::AtomicOrdering::SequentiallyConsistent,
        llvm::AtomicOrdering::SequentiallyConsistent);
    auto success = builder.CreateExtractValue(xchg, {1});
    builder.CreateCondBr(success, mark_success, add_to_mark_stack);
  }

  builder.SetInsertPoint(mark_success);
  // Load the MarkingStack block out of the thread. Then load top_ out of the
  // MarkingStackBlock and add the address to the pointers_.
  auto marking_stack_block = helper.EmitLoadFieldRaw(
      thread, kThreadMarkingStackBlockOffset, NonGCObjectPtrTy);
  auto marking_stack_top = helper.EmitLoadFieldRaw(
      marking_stack_block, kMarkingStackBlockTopOffset, Int32Ty);

  {
    auto offset = builder.CreateZExt(marking_stack_top, Int64Ty);
    offset = builder.CreateShl(offset, kWordSizeLog2);
    offset = builder.CreateAdd(
        offset, GetConstantInt(kMarkingStackBlockPointersOffset));
    helper.EmitStoreFieldRaw(marking_stack_block, offset, value);
  }

  auto marking_stack_overflow =
      helper.CreateBasicBlock("marking_stack_overflow");
  auto marking_stack_no_overflow =
      helper.CreateBasicBlock("marking_stack_no_overflow");

  {
    auto inc_top =
        builder.CreateAdd(marking_stack_top, GetConstantInt(1, Int32Ty));
    inc_top = builder.CreateTrunc(inc_top, Int32Ty);
    helper.EmitStoreFieldRaw(marking_stack_block, kMarkingStackBlockTopOffset,
                             inc_top);
    auto cmp = builder.CreateICmpEQ(
        inc_top, GetConstantInt(kMarkingStackBlockSize, Int32Ty));
    helper.EmitBrSlowFast(cmp, marking_stack_overflow,
                          marking_stack_no_overflow);
  }

  builder.SetInsertPoint(marking_stack_no_overflow);
  builder.CreateRetVoid();

  builder.SetInsertPoint(marking_stack_overflow);
  // Call to runtime.
  {
    auto func_ty = llvm::FunctionType::get(VoidTy, {NonGCObjectPtrTy}, false);
    auto func = llvm::Function::Create(
        func_ty, llvm::GlobalVariable::LinkageTypes::ExternalLinkage,
        "MarkingStackBlockProcess", module_);
    builder.CreateCall(func, {thread});
  }
  builder.CreateRetVoid();
}

void CodegenModule::GenerateGetStackMaps() {
  // Make the __LLVM_StackMaps section external.
  auto stack_maps = new llvm::GlobalVariable(
      GetModule(), Int8Ty, false, llvm::GlobalVariable::ExternalLinkage,
      nullptr, "__LLVM_StackMaps");

  auto func_ty = llvm::FunctionType::get(NonGCObjectPtrTy, {}, false);
  auto func =
      llvm::Function::Create(func_ty, llvm::GlobalVariable::ExternalLinkage,
                             "_LLVMGetStackMaps", module_);

  CodegenHelper helper(*this, func);
  auto bb = helper.CreateBasicBlock("entry");
  auto& builder = helper.builder();
  builder.SetInsertPoint(bb);
  builder.CreateRet(stack_maps);
}

llvm::Function* CodegenModule::GetOrCreateFixedParamTrampoline(
    size_t num_params) {
  auto it = fixed_params_trampolines_.find(num_params);
  if (it != fixed_params_trampolines_.end()) {
    return it->second;
  }
  auto trampoline = llvm::Function::Create(
      FixedParamTrampolineTy, llvm::GlobalVariable::InternalLinkage,
      "_FixedParamTrampoline$" + std::to_string(num_params), module_);

  trampoline->setGC(kGCStrategyName);

  CodegenHelper helper(*this, trampoline);
  auto bb = helper.CreateBasicBlock("entry");
  auto& builder = helper.builder();
  builder.SetInsertPoint(bb);

  llvm::Value* func_id = helper.GetParam(0);
  llvm::Value* thr = helper.GetParam(1);
  llvm::Value* args = helper.GetParam(2);

  std::vector<llvm::Value*> extracted_args{thr};
  for (size_t i = 0; i < num_params; ++i) {
    //  =======
    // | first |      ||
    // |-------|      ||
    // |       |      ||
    // |-------|      \/
    // |       |
    // |-------|
    // |       |
    // |-------|
    // | last  |
    //  -------  <--- args
    std::vector<llvm::Value*> idx{GetConstantInt(num_params - i)};
    auto argi = builder.CreateLoad(builder.CreateGEP(ObjectPtrTy, args, idx));
    extracted_args.push_back(argi);
  }

  auto callee_ty = helper.GetNonGCPointer(GetStaticFunctionType(num_params));
  auto callee =
      helper.EmitLoadFieldFromGlobal(function_pool_, func_id, callee_ty, true);
  builder.CreateRet(builder.CreateCall(callee, extracted_args));

  fixed_params_trampolines_.emplace(num_params, trampoline);
  return trampoline;
}

void CodegenModule::GenerateFunctions() {
  // Declarations
  std::vector<llvm::Constant*> function_pool_entries(program_->NumFunctions());
  for (size_t i = 0; i < program_->NumFunctions(); ++i) {
    auto func = program_->FunctionAt(i);

    auto linkage = llvm::GlobalVariable::InternalLinkage;
#ifdef TARGET_WASM
    // TODO(sarkin): Hack:
    // We call the functions directly on WASM.
    // Side-note: pretty much anything with ifdef Target_WASM is a hack :)
    linkage = llvm::GlobalVariable::ExternalLinkage;
#endif

    auto llvm_func =
        llvm::Function::Create(GetStaticFunctionType(func->num_params()),
                               linkage, "LLVM_" + func->name(), module_);
    if (func->can_trigger_gc()) {
      llvm_func->setGC(kGCStrategyName);
    }

    function_pool_entries[func->id()] =
        llvm::ConstantExpr::getBitCast(llvm_func, NonGCObjectPtrTy);

    function_by_id_.emplace(func->id(), llvm_func);
  }

  function_pool_->setInitializer(
      llvm::ConstantArray::get(FunctionPoolTy, function_pool_entries));

  std::vector<CodegenFunction> cgfs;
  for (size_t i = 0; i < program_->NumFunctions(); ++i) {
    auto func = program_->FunctionAt(i);

    auto llvm_func = GetFunctionByID(func->id());
    cgfs.emplace_back(*this, dispatch_table_.get(), func, llvm_func);
    cgfs.back().GenerateCode();
  }

  // TODO(sarkin): Should probably refactor this out.
  dispatch_table_->BuildTable();
  auto real_dispatch_table = llvm::ConstantExpr::getBitCast(
      dispatch_table_->dispatch_table(),
      llvm::PointerType::get(DispatchTableTy, kNonGCAddressSpace));
  temp_dispatch_table_->replaceAllUsesWith(real_dispatch_table);

  for (auto& cgf : cgfs) {
    cgf.PatchDynamicCalls();
  }
}

void CodegenModule::InitializeGlobals() {
  {
    std::vector<llvm::Constant*> init(
        program_->num_constants(), llvm::ConstantPointerNull::get(ObjectPtrTy));
    constant_pool_->setInitializer(
        llvm::ConstantArray::get(ConstantPoolTy, init));
  }

  {
    std::vector<llvm::Constant*> init(
        program_->num_dart_functions(),
        llvm::ConstantPointerNull::get(ObjectPtrTy));
    dart_function_pool_->setInitializer(
        llvm::ConstantArray::get(DartFunctionPoolTy, init));
  }

  {
    llvm_to_dart_trampoline_->setInitializer(
        llvm::ConstantPointerNull::get(NonGCObjectPtrTy));
  }
}

const std::string CodegenModule::kGCStrategyName = "statepoint-example";

const std::string CodegenModule::kGlobalConstantPoolName =
    "_GlobalConstantPool";
const std::string CodegenModule::kGlobalFunctionPoolName =
    "_GlobalFunctionPool";
const std::string CodegenModule::kGlobalDartFunctionPoolName =
    "_GlobalDartFunctionPool";
const std::string CodegenModule::kGlobalLLVMToDartTrampolineName =
    "_GlobalLLVMToDartTrampoline";
const std::string CodegenModule::kDartToLLVMTrampolineName =
    "_DartToLLVMTrampoline";

InstructionInputExtractor::InstructionInputExtractor(const ILInstruction* instr)
    : instr_(instr) {}

std::string InstructionInputExtractor::NextInput() {
  return instr_->InputAt(idx_++);
}

intptr_t InstructionInputExtractor::NextInputAsIntPtr() {
  return stoll(NextInput());
}

int64_t InstructionInputExtractor::NextInputAsInt64() {
  return stoll(NextInput());
}

template <typename T>
T InstructionInputExtractor::NextInputAsEnum() {
  return static_cast<T>(NextInputAsIntPtr());
}

size_t InstructionInputExtractor::NumInputs() const {
  return instr_->NumInputs();
}

size_t InstructionInputExtractor::NumInputsLeft() const {
  return NumInputs() - idx_;
}

CodegenHelper::CodegenHelper(CodegenModule& cgm, llvm::Function* function)
    : CodegenTypeCache(cgm),
      DartVMConstants(cgm),
      cgm_(cgm),
      function_(function),
      builder_(std::make_unique<llvm::IRBuilder<>>(cgm_.GetLLVMContext())) {}

llvm::Constant* CodegenHelper::GetConstantInt(int64_t val,
                                              llvm::IntegerType* ty) const {
  if (ty == nullptr) {
    ty = IntTy;
  }
  return llvm::ConstantInt::get(ty, val);
}

void CodegenHelper::SetInvariantLoad(llvm::LoadInst* val) {
  val->setMetadata(llvm::LLVMContext::MD_invariant_load,
                   llvm::MDNode::get(cgm_.GetLLVMContext(), llvm::None));
}

llvm::PointerType* CodegenHelper::GetNonGCPointer(llvm::Type* ty) {
  return llvm::PointerType::get(ty, CodegenModule::kNonGCAddressSpace);
}

llvm::Value* CodegenHelper::EmitLoadFieldFromGlobal(llvm::Value* global,
                                                    size_t offset,
                                                    llvm::Type* type,
                                                    bool invariant) {
  return EmitLoadFieldFromGlobal(global, GetConstantInt(offset), type,
                                 invariant);
}

llvm::Value* CodegenHelper::EmitLoadFieldFromGlobal(llvm::Value* global,
                                                    llvm::Value* offset,
                                                    llvm::Type* type,
                                                    bool invariant) {
  auto field_ptr_ty = GetNonGCPointer(type);

  auto field_ptr = builder_->CreateGEP(global, {GetConstantInt(0), offset});
  field_ptr = builder_->CreatePointerCast(field_ptr, field_ptr_ty);
  auto load = builder_->CreateLoad(field_ptr);
  if (invariant) {
    SetInvariantLoad(load);
  }
  return load;
}

llvm::Value* CodegenHelper::EmitBool(llvm::Value* val) {
  assert(val != nullptr);
  assert(val->getType() == Int1Ty);

  val = builder_->CreateZExt(val, Int64Ty);
  auto offset = builder_->CreateAdd(val, GetConstantInt(cgm_.FalseOffset()));
  return EmitLoadFieldFromGlobal(cgm_.constant_pool(), offset, ObjectPtrTy, true);
}

llvm::Value* CodegenHelper::EmitFalse() {
  return EmitBool(GetConstantInt(0, Int1Ty));
}

llvm::Value* CodegenHelper::EmitTrue() {
  return EmitBool(GetConstantInt(1, Int1Ty));
}

llvm::Value* CodegenHelper::EmitNull() {
  auto offset = GetConstantInt(cgm_.NullOffset());
  return EmitLoadFieldFromGlobal(cgm_.constant_pool(), offset, ObjectPtrTy, true);
}

llvm::Value* CodegenHelper::EmitTestBit(llvm::Value* val, int bit_num) {
  assert(bit_num < 30);
  auto bit = builder_->CreateAnd(val, (1 << bit_num));
  auto ty = val->getType();
  assert(ty->isIntegerTy());
  return builder_->CreateICmpNE(
      bit, GetConstantInt(0, llvm::cast<llvm::IntegerType>(ty)));
}

llvm::Value* CodegenHelper::EmitIsNewObject(llvm::Value* val) {
  val = EmitSmiToInt(val);
  return EmitTestBit(val, kNewObjectBitPosition);
}

void CodegenHelper::EmitBrSlowFast(llvm::Value* cond,
                                   llvm::BasicBlock* slow_path,
                                   llvm::BasicBlock* fast_path) {
  llvm::MDBuilder md_builder(cgm_.GetLLVMContext());
  auto assume_fast = md_builder.createBranchWeights(1, 100);
  builder_->CreateCondBr(cond, slow_path, fast_path, assume_fast);
}

void CodegenHelper::EmitBrFastSlow(llvm::Value* cond,
                                   llvm::BasicBlock* fast_path,
                                   llvm::BasicBlock* slow_path) {
  llvm::MDBuilder md_builder(cgm_.GetLLVMContext());
  auto assume_fast = md_builder.createBranchWeights(100, 1);
  builder_->CreateCondBr(cond, fast_path, slow_path, assume_fast);
}

llvm::Value* CodegenHelper::EmitNegateBool(llvm::Value* val) {
  auto cmp = builder_->CreateICmpEQ(val, EmitFalse());
  return EmitBool(cmp);
}

llvm::Value* CodegenHelper::EmitSmiToInt(llvm::Value* val) {
  assert(val->getType() == ObjectPtrTy);
  val = builder_->CreateAddrSpaceCast(val, NonGCObjectPtrTy);
  return builder_->CreatePtrToInt(val, Int64Ty);
}

llvm::Value* CodegenHelper::EmitIsAnyNotSmi(llvm::Value* a, llvm::Value* b) {
  assert(a->getType() == ObjectPtrTy);
  assert(b->getType() == ObjectPtrTy);
  
  a = EmitSmiToInt(a);
  b = EmitSmiToInt(b);

  auto ab_or = builder_->CreateOr(a, b);
  return builder_->CreateTrunc(ab_or, Int1Ty);
}

llvm::Value* CodegenHelper::EmitIsNotSmi(llvm::Value* val) {
  assert(val->getType() == ObjectPtrTy);
  // Checking for smi should be safe to do even in the presence
  // of a moving GC / compiler optimizations.
  val = EmitSmiToInt(val);
  return builder_->CreateTrunc(val, Int1Ty);
}

llvm::Value* CodegenHelper::EmitSmiTag(llvm::Value* val) {
  assert(val->getType() == Int64Ty);
  val = builder_->CreateAdd(val, val);
  return EmitNewObject(val);
}

llvm::Value* CodegenHelper::EmitUntagSmi(llvm::Value* val) {
  val = EmitSmiToInt(val);
  return builder_->CreateAShr(val, 1);
}

llvm::Value* CodegenHelper::EmitUntagObject(llvm::Value* obj) {
  return EmitFieldPointer(obj, -1, Int8Ty);
}

llvm::Value* CodegenHelper::EmitTagObject(llvm::Value* obj) {
  return EmitFieldPointer(obj, 1, Int8Ty);
}

llvm::Value* CodegenHelper::EmitFieldPointer(llvm::Value* obj,
                                             intptr_t offset,
                                             llvm::Type* ty) {
  return EmitFieldPointer(obj, GetConstantInt(offset), ty);
}

llvm::Value* CodegenHelper::EmitFieldPointer(llvm::Value* obj,
                                             llvm::Value* offset,
                                             llvm::Type* ty) {
  auto object_ty = llvm::cast<llvm::PointerType>(obj->getType());
  auto field_ptr_ty = llvm::PointerType::get(ty, object_ty->getAddressSpace());
  auto field_ptr = builder_->CreateGEP(obj, offset);
  return builder_->CreatePointerCast(field_ptr, field_ptr_ty);
}

llvm::Value* CodegenHelper::EmitNewObject(llvm::Value* ptr) {
  if (ptr->getType() != NonGCObjectPtrTy) {
    assert(ptr->getType() == Int64Ty);
    ptr = builder_->CreateIntToPtr(ptr, NonGCObjectPtrTy);
  }
  // TODO(sarkin): Currently the intrinsic takes an Int64Ty, which doesn't
  // work for 32-bit architectures. Add different intrinsics for different
  // pointer widths in LLVM, something like the uadd_with_overflow intrinsic
  // does.
#ifdef TARGET_X64
  return builder_->CreateIntrinsic(llvm::Intrinsic::dart_new_object, {}, {ptr});
#endif 
  return ptr;
}

llvm::Value* CodegenHelper::EmitNewObjectAndTag(llvm::Value* ptr) {
  assert(ptr->getType() == IntTy);
  // Tag.
  ptr = builder_->CreateAdd(ptr, GetConstantInt(1));
  return EmitNewObject(ptr);
}

llvm::Value* CodegenHelper::EmitBoxAllocation(size_t cid) {
  auto slow_path = CreateBasicBlock("box_slow_path");
  auto fast_path = CreateBasicBlock("box_fast_path");
  auto merge_path = CreateBasicBlock("box_merge_path");

  auto allocation_info = cgm_.GetClassAllocationInfo(cid);
  auto thread_object = GetThread();
  auto top = EmitLoadFieldRaw(thread_object, kThreadTopOffset, Int64Ty);
  auto size = GetConstantInt(allocation_info->instance_size());
  auto obj_end = builder_->CreateAdd(top, size);
  {
    auto end = EmitLoadFieldRaw(thread_object, kThreadEndOffset, Int64Ty);
    auto cmp = builder_->CreateICmpSGE(obj_end, end);
    EmitBrSlowFast(cmp, slow_path, fast_path);
  }

  builder_->SetInsertPoint(fast_path);
  EmitStoreFieldRaw(thread_object, kThreadTopOffset, obj_end);
  auto new_obj = EmitNewObjectAndTag(top);
  // 64-bit operation is intended to zero out higher bits.
  EmitStoreField(new_obj, kInstanceTagsOffset,
                 GetConstantInt(allocation_info->tags()));
  builder_->CreateBr(merge_path);

  builder_->SetInsertPoint(slow_path);
  auto stub = cgm_.GetOrCreateClassAllocationStub(cid);
  assert(allocation_info->class_object() >= 0);
  auto cls = EmitLoadConstant(allocation_info->class_object());
  auto slow_new_obj =
      builder_->CreateCall(stub, {GetThread(), cls, EmitNull()});
  builder_->CreateBr(merge_path);

  builder_->SetInsertPoint(merge_path);
  auto merge_obj = builder_->CreatePHI(ObjectPtrTy, 2);
  merge_obj->addIncoming(new_obj, fast_path);
  merge_obj->addIncoming(slow_new_obj, slow_path);

  return merge_obj;
}

llvm::Value* CodegenHelper::EmitStackArray(intptr_t size) {
  auto arg_array_ty = llvm::ArrayType::get(ObjectPtrTy, size);
  return builder_->CreateAlloca(arg_array_ty);
}

void CodegenHelper::EmitStoreInArray(llvm::Value* arr,
                                     intptr_t i,
                                     llvm::Value* val) {
  auto argi = builder_->CreateGEP(arr, {GetConstantInt(0), GetConstantInt(i)});
  builder_->CreateStore(val, argi);
}

llvm::Value* CodegenHelper::EmitStackArrayToPtr(llvm::Value* arr) {
  return builder_->CreateBitCast(arr, StackArrayTy);
}

llvm::Value* CodegenHelper::EmitNativeArguments(
    llvm::Value* ret_val,
    std::vector<llvm::Value*> args) {
  llvm::Value* args_array = nullptr;
  if (!args.empty()) {
    args_array = EmitStackArray(args.size());
    for (size_t i = 0; i < args.size(); ++i) {
      EmitStoreInArray(args_array, i, args[i]);
    }
    args_array = EmitStackArrayToPtr(args_array);
  }
  size_t argc_tags = args.size();
  assert(ArgcTagBits::kReverseArgOrderBit < 64);
  argc_tags |= (1ll << ArgcTagBits::kReverseArgOrderBit);
  return EmitNativeArguments(argc_tags, args_array, ret_val);
}

llvm::Value* CodegenHelper::EmitNativeArguments(intptr_t argc_tag,
                                                  llvm::Value* argv,
                                                  llvm::Value* ret_val) {
  auto native_args = builder_->CreateAlloca(NativeArgumentsTy);

  auto get_field = [&](int index) {
    std::vector<llvm::Value*> idx{GetConstantInt(/* GEP specific */ 0, Int32Ty),
                                  GetConstantInt(index, Int32Ty)};
    return builder_->CreateGEP(NativeArgumentsTy, native_args, idx);
  };

  auto thread_field = get_field(0);
  builder_->CreateStore(GetThread(), thread_field);
  auto argc_field = get_field(1);
  builder_->CreateStore(GetConstantInt(argc_tag), argc_field);
  if (argv != nullptr) {
    auto argv_field = get_field(2);
    builder_->CreateStore(argv, argv_field);
  }
  if (ret_val != nullptr) {
    auto ret_val_field = get_field(3);
    builder_->CreateStore(ret_val, ret_val_field);
  }

  return native_args;
}

void CodegenHelper::EmitCallToRuntimeTrampoline(
    CodegenModule::RuntimeEntryTag tag,
    llvm::Value* native_args) {
  auto call = builder_->CreateCall(
      cgm_.llvm_to_runtime_trampoline(),
      {GetThread(), GetConstantInt(kThreadTopExitFrameInfoOffset),
       GetConstantInt(tag), native_args});
  call->setCallingConv(llvm::CallingConv::C);
}

llvm::Value* CodegenHelper::EmitLoadConstant(intptr_t const_id) {
  std::vector<llvm::Value*> idx{GetConstantInt(/* GEP specific */ 0),
                                GetConstantInt(const_id)};

  // Invariant if loading False, True or Null.
  bool invariant = (const_id <= 2);
  return EmitLoadFieldFromGlobal(cgm_.constant_pool(), const_id, ObjectPtrTy,
                                 invariant);
}

llvm::Value* CodegenHelper::EmitLoadField(llvm::Value* object,
                                          intptr_t offset,
                                          llvm::Type* type) {
  return EmitLoadFieldRaw(EmitUntagObject(object), offset, type);
}

void CodegenHelper::EmitStoreField(llvm::Value* object,
                                   intptr_t offset,
                                   llvm::Value* val) {
  EmitStoreFieldRaw(EmitUntagObject(object), offset, val);
}

llvm::Value* CodegenHelper::EmitLoadField(llvm::Value* object,
                                          llvm::Value* offset,
                                          llvm::Type* type) {
  return EmitLoadFieldRaw(EmitUntagObject(object), offset, type);
}

void CodegenHelper::EmitStoreField(llvm::Value* object,
                                   llvm::Value* offset,
                                   llvm::Value* val) {
  EmitStoreFieldRaw(EmitUntagObject(object), offset, val);
}

llvm::Value* CodegenHelper::EmitLoadFieldRaw(llvm::Value* object,
                                             intptr_t offset,
                                             llvm::Type* type) {
  return EmitLoadFieldRaw(object, GetConstantInt(offset), type);
}

void CodegenHelper::EmitStoreFieldRaw(llvm::Value* object,
                                      intptr_t offset,
                                      llvm::Value* val) {
  EmitStoreFieldRaw(object, GetConstantInt(offset), val);
}

llvm::Value* CodegenHelper::EmitLoadFieldRaw(llvm::Value* object,
                                             llvm::Value* offset,
                                             llvm::Type* type) {
  auto field_ptr = EmitFieldPointer(object, offset, type);
  return builder_->CreateLoad(field_ptr);
}

void CodegenHelper::EmitStoreFieldRaw(llvm::Value* object,
                                      llvm::Value* offset,
                                      llvm::Value* val) {
  auto field_ptr = EmitFieldPointer(object, offset, val->getType());
  builder_->CreateStore(val, field_ptr);
}

void CodegenHelper::EmitStoreInObject(llvm::Value* object,
                                      intptr_t offset,
                                      llvm::Value* val,
                                      bool can_value_be_smi) {
  EmitStoreInObject(object, GetConstantInt(offset), val, can_value_be_smi);
}

void CodegenHelper::EmitStoreInObject(llvm::Value* object,
                                      llvm::Value* offset,
                                      llvm::Value* val,
                                      bool can_value_be_smi) {
  EmitStoreField(object, offset, val);

  auto exit_block = CreateBasicBlock("store_in_object_exit");
  if (can_value_be_smi) {
    auto val_is_not_smi = CreateBasicBlock("val_is_not_smi");
    auto cmp = EmitIsNotSmi(val);
    builder_->CreateCondBr(cmp, val_is_not_smi, exit_block);

    builder_->SetInsertPoint(val_is_not_smi);
  }

  auto object_tags = EmitLoadField(object, kObjectTagsOffset, Int8Ty);
  object_tags =
      builder_->CreateAShr(object_tags, kRawObjectBarrierOverlapShift);
  auto wb_mask =
      EmitLoadFieldRaw(GetThread(), kThreadWriteBarrierMaskOffset, Int8Ty);
  object_tags = builder_->CreateAnd(object_tags, wb_mask);
  auto val_tags = EmitLoadField(val, kObjectTagsOffset, Int8Ty);

  auto wb_block = CreateBasicBlock("wb");
  {
    auto and_tags = builder_->CreateAnd(object_tags, val_tags);
    auto cmp = builder_->CreateICmpEQ(and_tags, GetConstantInt(0, Int8Ty));
    builder_->CreateCondBr(cmp, exit_block, wb_block);
  }

  builder_->SetInsertPoint(wb_block);
  builder_->CreateCall(cgm_.write_barrier(), {GetThread(), object, val});
  builder_->CreateBr(exit_block);

  builder_->SetInsertPoint(exit_block);
}

llvm::Value* CodegenHelper::GetThread() const {
  // Thread is always the first parameter.
  return GetParam(CodegenFunction::kThreadOffset);
}

llvm::Value* CodegenHelper::GetParam(size_t i) const {
  assert(function_ != nullptr);
  assert(i < function_->arg_size());

  return function_->arg_begin() + i;
}

llvm::BasicBlock* CodegenHelper::CreateBasicBlock(intptr_t id) {
  return CreateBasicBlock(std::to_string(id));
}

llvm::BasicBlock* CodegenHelper::CreateBasicBlock(std::string id) {
  return llvm::BasicBlock::Create(cgm_.GetLLVMContext(), id, function_);
}

CodegenFunction::CodegenFunction(CodegenModule& cgm,
                                 DispatchTable* dispatch_table,
                                 const DartFunction* func,
                                 llvm::Function* llvm_func)
    : CodegenTypeCache(cgm),
      DartVMConstants(cgm),
      cgm_(cgm),
      dispatch_table_(dispatch_table),
      func_(func),
      helper_(cgm, llvm_func),
      builder_(helper_.builder()) {}

llvm::Constant* CodegenFunction::GetConstantInt(int64_t val,
                                                llvm::IntegerType* ty) const {
  return helper_.GetConstantInt(val, ty);
}

void CodegenFunction::GenerateCode() {
  assert(!is_generated_);
  is_generated_ = true;

  for (size_t i = 0; i < func_->NumBasicBlocks(); ++i) {
    auto bb = func_->BasicBlockAt(i);
    blocks_.emplace(bb->id(), helper_.CreateBasicBlock(bb->id()));
  }

  // Each Dart basic block might in turn be converted into many
  // LLVM basic blocks. The LLVM basic blocks corresponding
  // to one Dart basic block form a graph with a single entry
  // and a single exit. The unique entry is the basic block
  // created above (stored in blocks_), while the unique exits
  // are stored in final_blocks_.
  for (size_t i = 0; i < func_->NumBasicBlocks(); ++i) {
    auto bb = func_->BasicBlockAt(i);
    GenerateBasicBlock(bb, GetBlock(bb->id()));
    final_blocks_[bb->id()] = builder_.GetInsertBlock();
  }

  // Assume there's one function entry block and jump into
  // it from the entry block. Furthermore, assume the first
  // block is the entry and the second is the only function
  // entry block, which *should* be the case since blocks
  // are in reverse postorder.
  assert(func_->NumBasicBlocks() >= 2);
  const int kEntryBlock = func_->BasicBlockAt(0)->id();
  const int kFunctionEntryBlock = func_->BasicBlockAt(1)->id();
  builder_.SetInsertPoint(GetBlock(kEntryBlock));
  builder_.CreateBr(GetBlock(kFunctionEntryBlock));

  ProcessPhiIncomingValues();
}

void CodegenFunction::PatchDynamicCalls() {
  assert(is_generated_ && !is_patched_);
  is_patched_ = true;
  for (const auto& pp : dynamic_calls_to_patch_) {
    auto new_offset = GetConstantInt(dispatch_table_->GetOffset(pp.first));
    pp.second->replaceAllUsesWith(new_offset);
  }
}

void CodegenFunction::GenerateBasicBlock(const DartBasicBlock* bb,
                                         llvm::BasicBlock* llvm_bb) {
  builder_.SetInsertPoint(llvm_bb);

  for (size_t i = 0; i < bb->NumInstructions(); ++i) {
    EmitInstruction(bb->InstructionAt(i));
  }
}

llvm::Value* CodegenFunction::EmitGraphEntry(InstructionInputExtractor I) {
  assert(false);
  return nullptr;
}

llvm::Value* CodegenFunction::EmitFunctionEntry(InstructionInputExtractor I) {
  assert(false);
  return nullptr;
}

llvm::Value* CodegenFunction::EmitTargetEntry(InstructionInputExtractor I) {
  assert(false);
  return nullptr;
}

llvm::Value* CodegenFunction::EmitJoinEntry(InstructionInputExtractor I) {
  assert(false);
  return nullptr;
}

llvm::Value* CodegenFunction::EmitConstant(InstructionInputExtractor I) {
  auto is_smi = I.NextInputAsIntPtr();
  if (is_smi) {
    // Should already be tagged.
    auto val = I.NextInputAsIntPtr();
    assert(!(val & 1));
    return helper_.EmitNewObject(GetConstantInt(val));
  }
  auto const_id = I.NextInputAsIntPtr();
  return helper_.EmitLoadConstant(const_id);
}

llvm::Value* CodegenFunction::EmitReturn(InstructionInputExtractor I) {
  auto ret_val = I.NextInput();
  builder_.CreateRet(GetValue(ret_val));
  return nullptr;
}

llvm::Value* CodegenFunction::EmitGoto(InstructionInputExtractor I) {
  auto dest = I.NextInputAsIntPtr();
  builder_.CreateBr(GetBlock(dest));
  return nullptr;
}

llvm::Value* CodegenFunction::EmitParameter(InstructionInputExtractor I) {
  size_t param_idx = I.NextInputAsIntPtr();
  assert(param_idx >= 0 && param_idx < func_->num_params());
  return helper_.GetParam(param_idx + kDartParamOffset);
}

llvm::Value* CodegenFunction::EmitPhi(InstructionInputExtractor I) {
  auto representation = I.NextInputAsEnum<Representation>();
  auto phi_ty = GetTypeFromRepresentation(representation);
  // entry = (value_id, block_id)
  const int kEntrySize = 2;
  size_t num_income_values = I.NumInputsLeft();
  assert(num_income_values % kEntrySize == 0);
  num_income_values /= kEntrySize;
  assert(num_income_values > 1);
  // For processing later as needed values might not be emitted yet.
  phi_instructions_.push_back(I.instr());
  return builder_.CreatePHI(phi_ty, num_income_values);
}

llvm::Value* CodegenFunction::EmitUnboxedConstant(InstructionInputExtractor I) {
  auto representation = I.NextInputAsEnum<Representation>();
  if (representation == Representation::kUnboxedInt32 ||
      representation == Representation::kUnboxedInt64) {
    auto val = I.NextInputAsInt64();
    return GetConstantInt(val);
  } else {
    // TODO(sarkin): Other representations
    assert(false);
  }
  return nullptr;
}

llvm::Value* CodegenFunction::EmitIntegerArithmetic(TokenKind op,
                                                    llvm::Value* left,
                                                    llvm::Value* right) {
  assert(left->getType() == right->getType());
  assert(left->getType() == Int32Ty || left->getType() == Int64Ty);

  static const std::unordered_map<TokenKind, llvm::Instruction::BinaryOps>
      kDartOpToLLVM{{TokenKind::kADD, llvm::Instruction::BinaryOps::Add},
                    {TokenKind::kSUB, llvm::Instruction::BinaryOps::Sub},
                    {TokenKind::kMUL, llvm::Instruction::BinaryOps::Mul},
                    {TokenKind::kBIT_AND, llvm::Instruction::BinaryOps::And},
                    {TokenKind::kBIT_OR, llvm::Instruction::BinaryOps::Or},
                    {TokenKind::kBIT_XOR, llvm::Instruction::BinaryOps::Xor},
                    {TokenKind::kMOD,
                     llvm::Instruction::BinaryOps::SRem},  // TODO(sarkin): mod
                    {TokenKind::kTRUNCDIV,
                     llvm::Instruction::BinaryOps::SDiv}};  // TODO(sarkin): mod
  auto llvm_op = kDartOpToLLVM.find(op);
  assert(llvm_op != kDartOpToLLVM.end());

  return builder_.CreateBinOp(llvm_op->second, left, right);
}

llvm::Value* CodegenFunction::EmitBinaryInt64Op(InstructionInputExtractor I) {
  auto op = I.NextInputAsEnum<TokenKind>();
  auto left = GetValueOrConstant(I.NextInput());
  auto right = GetValueOrConstant(I.NextInput());

  assert(left->getType() == Int64Ty);
  assert(right->getType() == Int64Ty);

  return EmitIntegerArithmetic(op, left, right);
}

llvm::Value* CodegenFunction::EmitComparisonOpInt64(TokenKind op,
                                                    llvm::Value* left,
                                                    llvm::Value* right) {
  if (left->getType()->isPointerTy()) {
    left = helper_.EmitSmiToInt(left);
  }
  if (right->getType()->isPointerTy()) {
    right = helper_.EmitSmiToInt(right);
  }

  static const std::unordered_map<TokenKind, llvm::CmpInst::Predicate>
      kDartOpToLLVM{{TokenKind::kLT, llvm::CmpInst::Predicate::ICMP_SLT},
                    {TokenKind::kGT, llvm::CmpInst::Predicate::ICMP_SGT},
                    {TokenKind::kGTE, llvm::CmpInst::Predicate::ICMP_SGE},
                    {TokenKind::kLTE, llvm::CmpInst::Predicate::ICMP_SLE},
                    {TokenKind::kEQ, llvm::CmpInst::Predicate::ICMP_EQ},
                    {TokenKind::kNE, llvm::CmpInst::Predicate::ICMP_NE}};

  auto llvm_op = kDartOpToLLVM.find(op);
  assert(llvm_op != kDartOpToLLVM.end());
  return builder_.CreateICmp(llvm_op->second, left, right);
}

llvm::Value* CodegenFunction::EmitComparisonOpDouble(TokenKind op,
                                                     llvm::Value* left,
                                                     llvm::Value* right) {
  // Using ordered comparisons, i.e. having NaN as either operand results in false
  assert(left->getType()->isDoubleTy());
  assert(right->getType()->isDoubleTy());

  static const std::unordered_map<TokenKind, llvm::CmpInst::Predicate>
      kDartOpToLLVM{{TokenKind::kLT, llvm::CmpInst::Predicate::FCMP_OLT},
                    {TokenKind::kGT, llvm::CmpInst::Predicate::FCMP_OGT},
                    {TokenKind::kGTE, llvm::CmpInst::Predicate::FCMP_OGE},
                    {TokenKind::kLTE, llvm::CmpInst::Predicate::FCMP_OLE},
                    {TokenKind::kEQ, llvm::CmpInst::Predicate::FCMP_OEQ},
                    {TokenKind::kNE, llvm::CmpInst::Predicate::FCMP_ONE}};

  auto llvm_op = kDartOpToLLVM.find(op);
  assert(llvm_op != kDartOpToLLVM.end());
  return builder_.CreateICmp(llvm_op->second, left, right);
}

llvm::Value* CodegenFunction::EmitRelationalOp(InstructionInputExtractor I) {
  auto op = I.NextInputAsEnum<TokenKind>();
  auto op_type = I.NextInputAsEnum<RelationalOpCid>();
  auto left = GetValueOrConstant(I.NextInput());
  auto right = GetValueOrConstant(I.NextInput());
  llvm::Value* result = nullptr;
  switch (op_type) {
    case RelationalOpCid::kInt64:
      result = EmitComparisonOpInt64(op, left, right);
      break;
    case RelationalOpCid::kDouble:
      result = EmitComparisonOpDouble(op, left, right);
      break;
    default:
      assert(false);
  }
  if (!I.instr()->IsComparisonInBranch()) {
    result = helper_.EmitBool(result);
  }
  return result;
}

llvm::Value* CodegenFunction::EmitEqualityCompare(InstructionInputExtractor I) {
  return EmitRelationalOp(I);
}

llvm::Value* CodegenFunction::EmitStrictCompare(InstructionInputExtractor I) {
  // TODO(sarkin): Num checks.
  auto op = I.NextInputAsEnum<TokenKind>();
  bool needs_num_check = I.NextInputAsInt64();
  auto left = GetValue(I.NextInput());
  auto right = GetValue(I.NextInput());
  assert(!needs_num_check);

  auto cmp = (op == kNE_STRICT) ? builder_.CreateICmpNE(left, right)
                                : builder_.CreateICmpEQ(left, right);
  if (!I.instr()->IsComparisonInBranch()) {
    return helper_.EmitBool(cmp);
  }
  return cmp;
}

llvm::Value* CodegenFunction::EmitBranch(InstructionInputExtractor I) {
  // The -1 comes from the ComparisonInstr, which has a ssa_tmp_index -1 and
  // is processed right before the branch instruction
  auto cmp = GetValue("v-1");
  auto true_block = GetBlock(I.NextInputAsIntPtr());
  auto false_block = GetBlock(I.NextInputAsIntPtr());
  builder_.CreateCondBr(cmp, true_block, false_block);
  return nullptr;
}

llvm::Value* CodegenFunction::EmitBoxInt64(InstructionInputExtractor I) {
  auto val_fits_smi = I.NextInputAsIntPtr();
  auto val = GetValue(I.NextInput());
  if (val_fits_smi) {
    return helper_.EmitSmiTag(val);
  }

  auto fast_path = helper_.CreateBasicBlock("box64_fast_path");
  auto slow_path = helper_.CreateBasicBlock("box64_slow_path");
  auto merge_path = helper_.CreateBasicBlock("box64_merge_path");

  auto try_tag = builder_.CreateIntrinsic(
      llvm::Intrinsic::ID::sadd_with_overflow, {Int64Ty}, {val, val});
  auto tagged = builder_.CreateExtractValue(try_tag, {0});
  auto of = builder_.CreateExtractValue(try_tag, {1});
  helper_.EmitBrSlowFast(of, slow_path, fast_path);

  builder_.SetInsertPoint(fast_path);
  tagged = helper_.EmitNewObject(tagged);
  builder_.CreateBr(merge_path);

  builder_.SetInsertPoint(slow_path);
  auto mint = helper_.EmitBoxAllocation(kMintCid);
  auto mint_block = builder_.GetInsertBlock();
  helper_.EmitStoreField(mint, kMintValueOffset, val);
  builder_.CreateBr(merge_path);

  builder_.SetInsertPoint(merge_path);
  auto boxed = builder_.CreatePHI(ObjectPtrTy, 2);
  boxed->addIncoming(tagged, fast_path);
  boxed->addIncoming(mint, mint_block);

  return boxed;
}

llvm::Value* CodegenFunction::EmitUnboxInt64(InstructionInputExtractor I) {
  bool is_smi = I.NextInputAsIntPtr();
  auto val = GetValue(I.NextInput());
  if (is_smi) {
    return helper_.EmitUntagSmi(val);
  }

  auto smi_bb = helper_.CreateBasicBlock("unbox_smi");
  auto mint_bb = helper_.CreateBasicBlock("unbox_mint");
  auto merge_bb = helper_.CreateBasicBlock("unbox_merge");

  auto cmp = helper_.EmitIsNotSmi(val);
  helper_.EmitBrSlowFast(cmp, mint_bb, smi_bb);

  builder_.SetInsertPoint(smi_bb);
  auto unboxed_smi = helper_.EmitUntagSmi(val);
  builder_.CreateBr(merge_bb);

  builder_.SetInsertPoint(mint_bb);
  auto unboxed_mint = helper_.EmitLoadField(val, kMintValueOffset, Int64Ty);
  builder_.CreateBr(merge_bb);

  builder_.SetInsertPoint(merge_bb);
  auto unboxed_val = builder_.CreatePHI(Int64Ty, 2);
  unboxed_val->addIncoming(unboxed_smi, smi_bb);
  unboxed_val->addIncoming(unboxed_mint, mint_bb);

  return unboxed_val;
}

llvm::Value* CodegenFunction::EmitCheckStackOverflow(
    InstructionInputExtractor I) {
  // TODO(sarkin):  Implement the full semantics of CheckStackOverflow.
  auto error_bb = helper_.CreateBasicBlock("stack_error");
  auto cont_bb = helper_.CreateBasicBlock("stack_cont");

  auto sp = builder_.CreateCall(cgm_.read_sp());
  auto stack_limit = helper_.EmitLoadFieldRaw(helper_.GetThread(),
                                              kThreadStackLimitOffset, Int64Ty);
  auto cmp = builder_.CreateICmpULT(sp, stack_limit);
  helper_.EmitBrSlowFast(cmp, error_bb, cont_bb);

  builder_.SetInsertPoint(error_bb);
  // TODO(sarkin):
  EmitNullError();

  builder_.SetInsertPoint(cont_bb);

  return nullptr;
}

void CodegenFunction::EmitNullError() {
  auto native_args = helper_.EmitNativeArguments(nullptr, {});
  helper_.EmitCallToRuntimeTrampoline(
      CodegenModule::RuntimeEntryTag::kNullError, native_args);
  builder_.CreateUnreachable();
}

llvm::Value* CodegenFunction::EmitCheckNull(InstructionInputExtractor I) {
  auto val = GetValue(I.NextInput());
  auto error_bb = helper_.CreateBasicBlock("null_error");
  auto cont_bb = helper_.CreateBasicBlock("null_cont");

  auto null = helper_.EmitNull();

  auto cmp = builder_.CreateICmpEQ(val, null);
  helper_.EmitBrSlowFast(cmp, error_bb, cont_bb);

  builder_.SetInsertPoint(error_bb);
  EmitNullError();

  builder_.SetInsertPoint(cont_bb);

  return nullptr;
}

llvm::Value* CodegenFunction::EmitPushArgument(InstructionInputExtractor I) {
  return nullptr;
}

llvm::Value* CodegenFunction::EmitLLVMStaticCall(
    size_t llvm_function_id,
    const Selector& selector,
    const std::vector<std::string>& args) {
  std::vector<llvm::Value*> vargs{helper_.GetThread()};
  std::transform(args.begin(), args.end(), std::back_inserter(vargs),
                 [&](auto arg) { return GetValue(arg); });
  auto fn = cgm_.GetOrCreateHandleOptionalParamsTrampoline(llvm_function_id,
                                                           selector);
  return builder_.CreateCall(fn, vargs);
}

llvm::Value* CodegenFunction::EmitDartStaticCall(
    intptr_t dart_function_id,
    const Selector& selector,
    const std::vector<std::string>& args) {
  std::vector<llvm::Value*> vargs{helper_.GetThread()};
  std::transform(args.begin(), args.end(), std::back_inserter(vargs),
                 [&](auto arg) { return GetValue(arg); });
  auto fn = cgm_.GetOrCreateDartCallTrampoline(dart_function_id, selector);
  return builder_.CreateCall(fn, vargs);
}

llvm::Value* CodegenFunction::EmitSpecialParameter(
    InstructionInputExtractor I) {
  // TODO(sarkin): return null for now.
  return helper_.EmitNull();
}

namespace {
void ReadCall(InstructionInputExtractor I,
              intptr_t& arg_descriptor_id,
              std::vector<std::string>& named_args,
              std::vector<std::string>& args) {
  arg_descriptor_id = I.NextInputAsIntPtr();
  size_t num_named = I.NextInputAsIntPtr();
  named_args.reserve(num_named);
  for (size_t i = 0; i < num_named; ++i) {
    named_args.push_back(I.NextInput());
  }
  auto argc = I.NumInputsLeft();
  args.reserve(argc);
  for (size_t i = 0; i < argc; ++i) {
    args.push_back(I.NextInput());
  }
}
}  // namespace

llvm::Value* CodegenFunction::EmitStaticCall(InstructionInputExtractor I) {
  bool is_llvm_compiled = I.NextInputAsInt64();
  auto patch_point = I.NextInput();
  intptr_t arg_descriptor_id;
  std::vector<std::string> named_args;
  std::vector<std::string> args;
  ReadCall(I, arg_descriptor_id, named_args, args);

  Selector selector("", args.size(), std::move(named_args), arg_descriptor_id);
  if (!is_llvm_compiled) {
    auto dart_function_id = cgm_.GetDartFunctionIndexByPatchPoint(patch_point);
    return EmitDartStaticCall(dart_function_id, selector, args);
  }
  auto llvm_function_id = cgm_.GetFunctionIDByPatchPoint(patch_point);
  return EmitLLVMStaticCall(llvm_function_id, selector, args);
}

llvm::Value* CodegenFunction::EmitInstanceCall(InstructionInputExtractor I) {
  // TODO(sarkin):
  std::string name = I.NextInput();
  bool check_smi = I.NextInputAsInt64();
  intptr_t arg_descriptor_id;
  std::vector<std::string> named_args;
  std::vector<std::string> args;
  ReadCall(I, arg_descriptor_id, named_args, args);

  // TODO(sarkin): How to handle dyn: if they're not in the invocation cache?
  if (name.substr(0, 4) == "dyn:") {
    name = name.substr(4);
  }
  std::replace(name.begin(), name.end(), '@', '$');

  Selector selector(name, args.size(), std::move(named_args),
                    arg_descriptor_id);
  auto selector_id = dispatch_table_->AddSelector(selector);

  // We don't know the offset yet, because the dispatch table is still
  // being constructed. Return a dummy value and store it to be replaced
  // later with the real offset.
  // Note: Can't use a constant here, because they're uniqued across the
  // whole module. Create an alloca that will be optimised-out later.
  auto dummy_offset = builder_.CreateLoad(builder_.CreateAlloca(Int64Ty));
  dynamic_calls_to_patch_.emplace_back(selector.selector(), dummy_offset);

  std::vector<llvm::Value*> vargs{helper_.GetThread()};
  std::transform(args.begin(), args.end(), std::back_inserter(vargs),
                 [&](auto arg) { return GetValue(arg); });

  const int kReceiverIndex = 1;
  auto receiver = vargs[kReceiverIndex];
  auto receiver_cid = EmitCid(receiver, check_smi);
  auto entry_offset = builder_.CreateAdd(dummy_offset, receiver_cid);

  auto layout = cgm_.GetDataLayout().getStructLayout(DispatchTableEntryTy);
  auto struct_size = GetConstantInt(layout->getSizeInBytes());
  // Check that selector-id is at offset 0 in the struct.
  assert(layout->getElementOffset(0) == 0);
  auto selector_id_offset = builder_.CreateMul(entry_offset, struct_size);
  auto target_offset = builder_.CreateAdd(
      selector_id_offset, GetConstantInt(layout->getElementOffset(1)));

  auto get_element = [&](auto offset, auto type) {
    auto addr = builder_.CreatePtrToInt(cgm_.GetLLVMDispatchTable(), Int64Ty);
    addr = builder_.CreateAdd(addr, offset);
    auto elty = helper_.GetNonGCPointer(type);
    auto elp = builder_.CreateIntToPtr(addr, elty);
    return builder_.CreateLoad(elp);
  };

  auto fpty = helper_.GetNonGCPointer(cgm_.GetStaticFunctionType(args.size()));
  auto target = get_element(target_offset, fpty);
  auto nsm_block = helper_.CreateBasicBlock("instance_call_nsm");
  auto smi_safe = helper_.CreateBasicBlock("smi_safe");
  auto selector_safe = helper_.CreateBasicBlock("selector_safe");
  if (check_smi) {
    auto cmp = helper_.EmitIsNotSmi(receiver);
    helper_.EmitBrFastSlow(cmp, smi_safe, nsm_block);
  } else {
    builder_.CreateBr(smi_safe);
  }
  builder_.SetInsertPoint(smi_safe);
  {
    auto table_sid = get_element(selector_id_offset, Int64Ty);
    auto cmp = builder_.CreateICmpEQ(GetConstantInt(selector_id), table_sid);
    helper_.EmitBrFastSlow(cmp, selector_safe, nsm_block);
  }

  builder_.SetInsertPoint(nsm_block);
  // TODO(sarkin): No such method.
  builder_.CreateUnreachable();

  builder_.SetInsertPoint(selector_safe);

  return builder_.CreateCall(target, vargs);
}

llvm::Value* CodegenFunction::EmitLoadIndexedUnsafe(
    InstructionInputExtractor I) {
  return EmitParameter(I);
}

llvm::Value* CodegenFunction::EmitPolymorphicInstanceCall(
    InstructionInputExtractor I) {
  return EmitInstanceCall(I);
}

llvm::Value* CodegenFunction::EmitLoadClassId(InstructionInputExtractor I) {
  bool check_smi = I.NextInputAsInt64();
  auto val = GetValue(I.NextInput());
  return helper_.EmitSmiTag(EmitCid(val, check_smi));
}

llvm::Value* CodegenFunction::EmitLoadStaticField(InstructionInputExtractor I) {
  auto val = GetValue(I.NextInput());
  return helper_.EmitLoadField(val, kFieldStaticValueOffset, ObjectPtrTy);
}

void CodegenFunction::EmitNonBoolTypeError(llvm::Value* val) {
  auto native_args = helper_.EmitNativeArguments(nullptr, {val});
  helper_.EmitCallToRuntimeTrampoline(
      CodegenModule::RuntimeEntryTag::kNonBoolTypeError, native_args);
}

llvm::Value* CodegenFunction::EmitAssertBoolean(InstructionInputExtractor I) {
  auto val = GetValue(I.NextInput());
  auto done_bb = helper_.CreateBasicBlock("done");
  auto not_false_bb = helper_.CreateBasicBlock("not_false");
  auto not_bool_bb = helper_.CreateBasicBlock("not_bool");

  auto false_obj = helper_.EmitBool(GetConstantInt(0, Int1Ty));

  auto cmp = builder_.CreateICmpEQ(val, false_obj);
  builder_.CreateCondBr(cmp, done_bb, not_false_bb);

  builder_.SetInsertPoint(not_false_bb);
  auto true_obj = helper_.EmitBool(GetConstantInt(1, Int1Ty));
  cmp = builder_.CreateICmpEQ(val, true_obj);
  helper_.EmitBrFastSlow(cmp, done_bb, not_bool_bb);

  builder_.SetInsertPoint(not_bool_bb);
  EmitNonBoolTypeError(val);
  builder_.CreateBr(done_bb);

  builder_.SetInsertPoint(done_bb);
  return nullptr;
}

llvm::Value* CodegenFunction::EmitCheckedSmiComparison(
    InstructionInputExtractor I) {
  auto op = I.NextInputAsEnum<TokenKind>();
  auto selector = I.NextInput();
  bool is_negated = I.NextInputAsIntPtr();
  bool is_left_smi = I.NextInputAsIntPtr();
  bool is_right_smi = I.NextInputAsIntPtr();
  auto left = GetValue(I.NextInput());
  auto right = GetValue(I.NextInput());

  llvm::Value* cmp = nullptr;
  if (left == right) {
    cmp = helper_.EmitIsNotSmi(left);
  } else if (is_left_smi) {
    cmp = helper_.EmitIsNotSmi(right);
  } else if (is_right_smi) {
    cmp = helper_.EmitIsNotSmi(left);
  } else {
    cmp = helper_.EmitIsAnyNotSmi(left, right);
  }
  auto slow_path = helper_.CreateBasicBlock("checked_smi_slow_path");
  auto fast_path = helper_.CreateBasicBlock("checked_smi_fast_path");
  auto merge_paths = helper_.CreateBasicBlock("checked_smi_merge_paths");
  helper_.EmitBrSlowFast(cmp, slow_path, fast_path);

  builder_.SetInsertPoint(slow_path);
  auto object_cmp = EmitInstanceCall(I);
  auto cmp_against = helper_.EmitBool(GetConstantInt(!is_negated, Int1Ty));
  auto slow_cmp = builder_.CreateICmpEQ(object_cmp, cmp_against);
  builder_.CreateBr(merge_paths);

  builder_.SetInsertPoint(fast_path);
  auto fast_cmp = EmitComparisonOpInt64(op, left, right);
  builder_.CreateBr(merge_paths);

  builder_.SetInsertPoint(merge_paths);
  auto merge_cmp = builder_.CreatePHI(Int1Ty, 2);
  merge_cmp->addIncoming(slow_cmp, slow_path);
  merge_cmp->addIncoming(fast_cmp, fast_path);

  if (!I.instr()->IsComparisonInBranch()) {
    return helper_.EmitBool(merge_cmp);
  }
  return merge_cmp;
}

llvm::Value* CodegenFunction::EmitCheckedSmiOp(
    InstructionInputExtractor I) {
  auto op = I.NextInputAsEnum<TokenKind>();
  auto selector = I.NextInput();
  bool is_left_smi = I.NextInputAsIntPtr();
  bool is_right_smi = I.NextInputAsIntPtr();
  auto left = GetValue(I.NextInput());
  auto right = GetValue(I.NextInput());

  llvm::Value* cmp = nullptr;
  if (left == right) {
    cmp = helper_.EmitIsNotSmi(left);
  } else if (is_left_smi) {
    cmp = helper_.EmitIsNotSmi(right);
  } else if (is_right_smi) {
    cmp = helper_.EmitIsNotSmi(left);
  } else {
    cmp = helper_.EmitIsAnyNotSmi(left, right);
  }
  auto slow_path = helper_.CreateBasicBlock("checked_smi_slow_path");
  auto fast_path = helper_.CreateBasicBlock("checked_smi_fast_path");
  auto merge_paths = helper_.CreateBasicBlock("checked_smi_merge_paths");
  helper_.EmitBrSlowFast(cmp, slow_path, fast_path);

  builder_.SetInsertPoint(slow_path);
  auto slow_val = EmitInstanceCall(I);
  builder_.CreateBr(merge_paths);

  builder_.SetInsertPoint(fast_path);
  left = helper_.EmitSmiToInt(left);
  right = helper_.EmitSmiToInt(right);
  auto fast_val = EmitIntegerArithmetic(op, left, right);
  fast_val = helper_.EmitNewObject(fast_val);
  builder_.CreateBr(merge_paths);

  builder_.SetInsertPoint(merge_paths);
  auto merge_val = builder_.CreatePHI(ObjectPtrTy, 2);
  merge_val->addIncoming(slow_val, slow_path);
  merge_val->addIncoming(fast_val, fast_path);

  return merge_val;
}

llvm::Value* CodegenFunction::EmitAssertAssignable(
    InstructionInputExtractor I) {
  // TODO(sarkin):
  return GetConstantInt(-1);
}

llvm::Value* CodegenFunction::EmitAllocateObject(InstructionInputExtractor I) {
  auto cid = I.NextInputAsIntPtr();
  auto cls = helper_.EmitLoadConstant(I.NextInputAsIntPtr());
  auto type_arguments = helper_.EmitNull();
  if (I.NumInputsLeft()) {
    type_arguments = GetValue(I.NextInput());
  }
  auto allocation_stub = cgm_.GetOrCreateClassAllocationStub(cid);
  return builder_.CreateCall(allocation_stub,
                             {helper_.GetThread(), cls, type_arguments});
}

llvm::Value* CodegenFunction::EmitCreateArray(InstructionInputExtractor I) {
  auto n = GetValue(I.NextInput());
  auto element_type = GetValue(I.NextInput());

  return builder_.CreateCall(cgm_.create_array_stub(),
                             {helper_.GetThread(), n, element_type});
}

llvm::Value* CodegenFunction::EmitLoadField(InstructionInputExtractor I) {
  bool is_unboxed_load = I.NextInputAsIntPtr();
  bool is_potential_unboxed = I.NextInputAsIntPtr();
  auto representation = I.NextInputAsEnum<Representation>();
  auto offset_in_bytes = I.NextInputAsIntPtr();
  auto instance = GetValue(I.NextInput());
  auto field_id = I.NextInputAsIntPtr();

  if (is_unboxed_load) {
    assert(representation != kTagged);
    intptr_t value_offset = 0;
    switch (representation) {
      case kUnboxedDouble:
        value_offset = kDoubleValueOffset;
        break;
      case kUnboxedFloat32x4:
        value_offset = kFloat32x4ValueOffset;
        break;
      case kUnboxedFloat64x2:
        value_offset = kFloat64x2ValueOffset;
        break;
      default:
        assert(false);
    }
    auto ty = GetTypeFromRepresentation(representation);
    auto field = helper_.EmitLoadField(instance, offset_in_bytes, ObjectPtrTy);
    return helper_.EmitLoadField(field, value_offset, ty);
  }

  auto load_pointer = helper_.CreateBasicBlock("load_pointer");

  if (is_potential_unboxed) {
    auto check_double = helper_.CreateBasicBlock("check_double");
    auto check_float32x4 = helper_.CreateBasicBlock("check_float32x4");
    auto check_float64x2 = helper_.CreateBasicBlock("check_float64x2");

    auto load_double = helper_.CreateBasicBlock("load_double");
    auto load_float32x4 = helper_.CreateBasicBlock("load_float32x4");
    auto load_float64x2 = helper_.CreateBasicBlock("load_float64x2");

    auto merge = helper_.CreateBasicBlock("merge_loads");

    auto field = helper_.EmitLoadConstant(field_id);

    auto is_nullable =
        helper_.EmitLoadField(field, kFieldIsNullableOffset, Int16Ty);
    {
      auto cmp =
          builder_.CreateICmpEQ(is_nullable, GetConstantInt(kNullCid, Int16Ty));
      builder_.CreateCondBr(cmp, load_pointer, check_double);
    }

    auto pointer_field =
        helper_.EmitLoadField(instance, offset_in_bytes, ObjectPtrTy);

    builder_.SetInsertPoint(check_double);
    auto guarded_cid =
        helper_.EmitLoadField(field, kFieldGuardedCidOffset, Int16Ty);
    {
      auto cmp = builder_.CreateICmpEQ(guarded_cid,
                                       GetConstantInt(kDoubleCid, Int16Ty));
      builder_.CreateCondBr(cmp, load_double, check_float32x4);
    }

    builder_.SetInsertPoint(check_float32x4);
    {
      auto cmp = builder_.CreateICmpEQ(guarded_cid,
                                       GetConstantInt(kFloat32x4Cid, Int16Ty));
      builder_.CreateCondBr(cmp, load_float32x4, check_float64x2);
    }

    builder_.SetInsertPoint(check_float64x2);
    {
      auto cmp = builder_.CreateICmpEQ(guarded_cid,
                                       GetConstantInt(kFloat64x2Cid, Int16Ty));
      builder_.CreateCondBr(cmp, load_float64x2, load_pointer);
    }

    builder_.SetInsertPoint(load_pointer);
    builder_.CreateBr(merge);

    builder_.SetInsertPoint(load_double);
    auto double_field = helper_.EmitBoxAllocation(kDoubleCid);
    load_double = builder_.GetInsertBlock();
    {
      auto val = helper_.EmitLoadField(load_pointer, kDoubleValueOffset,
                                             DoubleTy);
      helper_.EmitStoreField(double_field, kDoubleValueOffset, val);
    }
    builder_.CreateBr(merge);

    builder_.SetInsertPoint(load_float32x4);
    auto float32x4_field = helper_.EmitBoxAllocation(kFloat32x4Cid);
    load_float32x4 = builder_.GetInsertBlock();
    {
      auto val = helper_.EmitLoadField(load_pointer, kFloat32x4ValueOffset,
                                       Float32x4Ty);
      helper_.EmitStoreField(float32x4_field, kFloat32x4ValueOffset, val);
    }
    builder_.CreateBr(merge);

    builder_.SetInsertPoint(load_float64x2);
    auto float64x2_field = helper_.EmitBoxAllocation(kFloat64x2Cid);
    load_float64x2 = builder_.GetInsertBlock();
    {
      auto val = helper_.EmitLoadField(load_pointer, kFloat64x2ValueOffset,
                                       Float64x2Ty);
      helper_.EmitStoreField(float64x2_field, kFloat64x2ValueOffset, val);
    }
    builder_.CreateBr(merge);

    builder_.SetInsertPoint(merge);
    auto merge_field = builder_.CreatePHI(ObjectPtrTy, 4);
    merge_field->addIncoming(pointer_field, load_pointer);
    merge_field->addIncoming(double_field, load_double);
    merge_field->addIncoming(float32x4_field, load_float32x4);
    merge_field->addIncoming(float64x2_field, load_float64x2);

    return merge_field;
  } 

  builder_.CreateBr(load_pointer);

  builder_.SetInsertPoint(load_pointer);
  return helper_.EmitLoadField(instance, offset_in_bytes, ObjectPtrTy);
}

llvm::Value* CodegenFunction::EmitStoreInstanceField(InstructionInputExtractor I) {
  bool is_unboxed_store = I.NextInputAsIntPtr();
  bool is_initialization = I.NextInputAsIntPtr();
  bool is_potential_unboxed = I.NextInputAsIntPtr();
  bool should_emit_store_barrier = I.NextInputAsIntPtr();
  auto instance = GetValue(I.NextInput());
  auto value = GetValue(I.NextInput());
  auto unboxed_field_cid = I.NextInputAsIntPtr();
  auto offset_in_bytes = I.NextInputAsIntPtr();
  bool can_value_be_smi = I.NextInputAsIntPtr();
  auto field_id = I.NextInputAsIntPtr();

  if (is_unboxed_store) {
    llvm::Value* field = nullptr;
    if (is_initialization) {
      field = helper_.EmitBoxAllocation(unboxed_field_cid);
      helper_.EmitStoreInObject(instance, offset_in_bytes, field, false);
    } else {
      field = helper_.EmitLoadField(instance, offset_in_bytes, ObjectPtrTy);
    }

    auto val_offset = GetValueOffsetForCid(unboxed_field_cid);
    // TODO(sarkin): Might need to cast value to ty here,
    // e.g. if it's of type ObjectPtrTy.
    // auto ty = GetUnboxedTypeForCid(unboxed_field_cid);
    helper_.EmitStoreField(field, val_offset, value);
    return nullptr;
  }

  auto store_pointer_entry = helper_.CreateBasicBlock("store_pointer");
  llvm::BasicBlock* store_pointer_exit = nullptr;
  {
    auto cur_bb = builder_.GetInsertBlock();

    builder_.SetInsertPoint(store_pointer_entry);
    if (should_emit_store_barrier) {
      helper_.EmitStoreInObject(instance, offset_in_bytes, value,
                                can_value_be_smi);
    } else {
      assert(value->getType() == ObjectPtrTy);
      helper_.EmitStoreField(instance, offset_in_bytes, value);
    }
    store_pointer_exit = builder_.GetInsertBlock();

    builder_.SetInsertPoint(cur_bb);
  }

  if (is_potential_unboxed) {
    auto check_unboxing_candidate = helper_.CreateBasicBlock("check_unboxing_candidate");
    auto check_double = helper_.CreateBasicBlock("check_double");
    auto check_float32x4 = helper_.CreateBasicBlock("check_float32x4");
    auto check_float64x2 = helper_.CreateBasicBlock("check_float64x2");

    auto store_double = helper_.CreateBasicBlock("store_double");
    auto store_float32x4 = helper_.CreateBasicBlock("store_float32x4");
    auto store_float64x2 = helper_.CreateBasicBlock("store_float64x2");

    auto merge = helper_.CreateBasicBlock("merge_stores");

    auto field = helper_.EmitLoadConstant(field_id);

    auto is_nullable =
        helper_.EmitLoadField(field, kFieldIsNullableOffset, Int16Ty);
    {
      auto cmp =
          builder_.CreateICmpEQ(is_nullable, GetConstantInt(kNullCid, Int16Ty));
      builder_.CreateCondBr(cmp, store_pointer_entry, check_unboxing_candidate);
    }

    auto kind_bits = helper_.EmitLoadField(field, kFieldKindBitsOffset, Int8Ty);
    {
      auto cmp = helper_.EmitTestBit(kind_bits, kFieldUnboxingCandidateBit);
      builder_.CreateCondBr(cmp, check_unboxing_candidate, store_pointer_entry);
    }

    auto box_field =
        helper_.EmitLoadField(instance, offset_in_bytes, ObjectPtrTy);

    builder_.SetInsertPoint(check_double);
    auto guarded_cid =
        helper_.EmitLoadField(field, kFieldGuardedCidOffset, Int16Ty);
    {
      auto cmp = builder_.CreateICmpEQ(guarded_cid,
                                       GetConstantInt(kDoubleCid, Int16Ty));
      builder_.CreateCondBr(cmp, store_double, check_float32x4);
    }

    builder_.SetInsertPoint(check_float32x4);
    {
      auto cmp = builder_.CreateICmpEQ(guarded_cid,
                                       GetConstantInt(kFloat32x4Cid, Int16Ty));
      builder_.CreateCondBr(cmp, store_float32x4, check_float64x2);
    }

    builder_.SetInsertPoint(check_float64x2);
    {
      auto cmp = builder_.CreateICmpEQ(guarded_cid,
                                       GetConstantInt(kFloat64x2Cid, Int16Ty));
      builder_.CreateCondBr(cmp, store_float64x2, store_pointer_entry);
    }

    builder_.SetInsertPoint(store_pointer_exit);
    builder_.CreateBr(merge);

    auto null = helper_.EmitNull();
    auto ensure_is_box = [&](auto box, auto cid) {
      auto is_null = helper_.CreateBasicBlock("ensure_box_is_null");
      auto is_not_null = helper_.CreateBasicBlock("ensure_box_is_not_null");
      auto merge = helper_.CreateBasicBlock("ensure_box_merge");
      auto cmp = builder_.CreateICmpEQ(box, null);
      builder_.CreateCondBr(cmp, is_null, is_not_null);

      builder_.SetInsertPoint(is_null);
      auto new_box = helper_.EmitBoxAllocation(cid);
      helper_.EmitStoreInObject(instance, offset_in_bytes, new_box, false);
      is_not_null = builder_.GetInsertBlock();
      builder_.CreateBr(merge);

      builder_.SetInsertPoint(is_not_null);
      builder_.CreateBr(merge);

      builder_.SetInsertPoint(merge);
      auto merge_box = builder_.CreatePHI(ObjectPtrTy, 2);
      merge_box->addIncoming(box, is_not_null);
      merge_box->addIncoming(new_box, is_null);

      return merge_box;
    };

    builder_.SetInsertPoint(store_double);
    {
      box_field = ensure_is_box(box_field, kDoubleCid);
      auto val = helper_.EmitLoadField(value, kDoubleValueOffset, DoubleTy);
      helper_.EmitStoreField(box_field, kDoubleValueOffset, val);
    }
    builder_.CreateBr(merge);

    builder_.SetInsertPoint(store_float32x4);
    {
      box_field = ensure_is_box(box_field, kFloat32x4Cid);
      auto val =
          helper_.EmitLoadField(value, kFloat32x4ValueOffset, Float32x4Ty);
      helper_.EmitStoreField(box_field, kFloat32x4ValueOffset, val);
    }
    builder_.CreateBr(merge);

    builder_.SetInsertPoint(store_float64x2);
    {
      box_field = ensure_is_box(box_field, kFloat64x2Cid);
      auto val =
          helper_.EmitLoadField(value, kFloat64x2ValueOffset, Float64x2Ty);
      helper_.EmitStoreField(box_field, kFloat64x2ValueOffset, val);
    }
    builder_.CreateBr(merge);

    builder_.SetInsertPoint(merge);

    return nullptr;
  }

  builder_.CreateBr(store_pointer_entry);

  builder_.SetInsertPoint(store_pointer_exit);

  return nullptr;
}

llvm::Value* CodegenFunction::EmitLoadIndexed(InstructionInputExtractor I) {
  bool is_external = I.NextInputAsIntPtr();
  auto representation = I.NextInputAsEnum<Representation>();
  auto index_scale = I.NextInputAsIntPtr();
  auto class_id = I.NextInputAsIntPtr();
  intptr_t data_offset = 0;
  if (!is_external) {
    data_offset = I.NextInputAsIntPtr();
  }
  auto array = GetValue(I.NextInput());
  auto index = GetValueOrConstant(I.NextInput());
  // Check for Smi
  if (index->getType()->isPointerTy()) {
    if (index_scale == 1) {
      index = helper_.EmitUntagSmi(index);
    } else {
      // The smi index is either untagged (element size == 1), or it is left smi
      // tagged (for all element sizes > 1).
      index = helper_.EmitSmiToInt(index);
    }
  }

  // Note that index is expected smi-tagged, (i.e, times 2) for all arrays with
  // index scale factor > 1. E.g., for Uint8Array and OneByteString the index is
  // expected to be untagged before accessing.
  if (!(index_scale & 1)) {
    index_scale >>= 1;
  }
  auto offset = builder_.CreateMul(index, GetConstantInt(index_scale));
  offset = builder_.CreateAdd(offset, GetConstantInt(data_offset));

  auto ty = GetTypeFromRepresentation(representation);
  if (representation == kUnboxedDouble) {
    assert(class_id == kTypedDataFloat32ArrayCid ||
           class_id == kTypedDataFloat64ArrayCid);
    if (class_id == kTypedDataFloat32ArrayCid) {
      ty = FloatTy;
    }
  }

  auto get_element = [&](auto ty) {
    if (is_external) {
      return helper_.EmitLoadFieldRaw(array, offset, ty);
    }
    return helper_.EmitLoadField(array, offset, ty);
  };

  if (representation != kTagged) {
    llvm::Value* el = get_element(ty);
    if (ty->isIntegerTy()) {
      el = builder_.CreateSExt(el, Int64Ty);
    } else if (ty->isFloatTy()) {
      el = builder_.CreateFPExt(el, DoubleTy);
    }

    return el;
  }

  if (class_id == kTypedDataInt8ArrayCid) {
    auto el = get_element(Int8Ty);
    el = builder_.CreateSExt(el, Int64Ty);
    return helper_.EmitSmiTag(el);
  }

  if (class_id == kTypedDataUint8ArrayCid ||
      class_id == kTypedDataUint8ClampedArrayCid ||
      class_id == kExternalTypedDataUint8ArrayCid ||
      class_id == kExternalTypedDataUint8ClampedArrayCid ||
      class_id == kOneByteStringCid || class_id == kExternalOneByteStringCid) {
    auto el = get_element(Int8Ty);
    el = builder_.CreateZExt(el, Int64Ty);
    return helper_.EmitSmiTag(el);
  }

  if (class_id == kTypedDataInt16ArrayCid) {
    auto el = get_element(Int16Ty);
    el = builder_.CreateSExt(el, Int64Ty);
    return helper_.EmitSmiTag(el);
  }

  if (class_id == kTypedDataUint16ArrayCid || class_id == kTwoByteStringCid ||
      class_id == kExternalTwoByteStringCid) {
    auto el = get_element(Int16Ty);
    el = builder_.CreateZExt(el, Int64Ty);
    return helper_.EmitSmiTag(el);
  }

  assert(class_id == kArrayCid || class_id == kImmutableArrayCid);
  return get_element(ObjectPtrTy);
}

llvm::Value* CodegenFunction::EmitStoreIndexed(InstructionInputExtractor I) {
  // TODO(sarkin): Duplicated code with LoadIndexed.
  bool should_emit_store_barrier = I.NextInputAsIntPtr();
  bool is_external = I.NextInputAsIntPtr();
  auto index_scale = I.NextInputAsIntPtr();
  auto class_id = I.NextInputAsIntPtr();
  intptr_t data_offset = 0;
  if (!is_external) {
    data_offset = I.NextInputAsIntPtr();
  }
  bool can_value_be_smi = I.NextInputAsIntPtr();
  auto array = GetValue(I.NextInput());
  auto index = GetValueOrConstant(I.NextInput());
  auto value = GetValue(I.NextInput());
  bool is_constant = I.NextInputAsIntPtr();
  llvm::Value* const_val =
      (is_constant) ? GetValueOrConstant(I.NextInput()) : nullptr;

  // Check for Smi
  if (index->getType()->isPointerTy()) {
    if (index_scale == 1) {
      index = helper_.EmitUntagSmi(index);
    } else {
      // The smi index is either untagged (element size == 1), or it is left smi
      // tagged (for all element sizes > 1).
      index = helper_.EmitSmiToInt(index);
    }
  }

  // Note that index is expected smi-tagged, (i.e, times 2) for all arrays with
  // index scale factor > 1. E.g., for Uint8Array and OneByteString the index is
  // expected to be untagged before accessing.
  if (!(index_scale & 1)) {
    index_scale >>= 1;
  }
  auto offset = builder_.CreateMul(index, GetConstantInt(index_scale));
  offset = builder_.CreateAdd(offset, GetConstantInt(data_offset));

  if (class_id == kArrayCid) {
    if (should_emit_store_barrier) {
      helper_.EmitStoreInObject(array, offset, value, can_value_be_smi);
    } else {
      assert(value->getType() == ObjectPtrTy);
      helper_.EmitStoreField(array, offset, value);
    }
    return nullptr;
  }

  auto put_element = [&](auto val) {
    if (is_external) {
      return helper_.EmitStoreFieldRaw(array, offset, val);
    }
    return helper_.EmitStoreField(array, offset, val);
  };

  if (class_id == kTypedDataInt8ArrayCid ||
      class_id == kTypedDataUint8ArrayCid ||
      class_id == kExternalTypedDataUint8ArrayCid ||
      class_id == kOneByteStringCid) {
    value = helper_.EmitUntagSmi(value);
    auto final_val =
        builder_.CreateTrunc(((is_constant) ? const_val : value), Int8Ty);
    put_element(final_val);
    return nullptr;
  }

  if (class_id == kTypedDataUint8ClampedArrayCid ||
      class_id == kExternalTypedDataUint8ClampedArrayCid) {
  }

  if (class_id == kTypedDataInt16ArrayCid ||
      class_id == kTypedDataUint16ArrayCid) {
    value = helper_.EmitUntagSmi(value);
    auto final_val = builder_.CreateTrunc(value, Int16Ty);
    put_element(final_val);
    return nullptr;
  }

  if (class_id == kTypedDataInt32ArrayCid ||
      class_id == kTypedDataUint32ArrayCid) {
    auto final_val = builder_.CreateTrunc(value, Int32Ty);
    put_element(final_val);
    return nullptr;
  }

  if (class_id == kTypedDataInt64ArrayCid ||
      class_id == kTypedDataUint64ArrayCid) {
    put_element(value);
    return nullptr;
  }

  // TODO(sarkin):
  if (class_id == kTypedDataFloat32ArrayCid) {
    // auto final_val = builder_.
  }
  assert(false);
  return nullptr;
}

llvm::Value* CodegenFunction::EmitOneByteStringFromCharCode(
    InstructionInputExtractor I) {
  auto char_code = GetValue(I.NextInput());

  // char_code is Smi.
  char_code = helper_.EmitSmiToInt(char_code);

  // TODO(sarkin): Can the symbol table be moved by the GC?
  // If so, then should change to ObjectPtrTy.
  auto pre_symbols_table = helper_.EmitLoadFieldRaw(
      helper_.GetThread(), kThreadPredefinedSymbolsAddressOffset,
      NonGCObjectPtrTy);
  auto offset_in_table = builder_.CreateShl(char_code, kWordSizeLog2 - 1);
  offset_in_table = builder_.CreateAdd(
      offset_in_table,
      GetConstantInt(kWordSize * kSymbolNullCharCodeSymbolOffset));
  auto symbol =
      helper_.EmitLoadFieldRaw(pre_symbols_table, offset_in_table, ObjectPtrTy);
  return symbol;
}

void CodegenFunction::EmitRangeError(llvm::Value* index, llvm::Value* length) {
  auto native_args = helper_.EmitNativeArguments(nullptr, {length, index});
  helper_.EmitCallToRuntimeTrampoline(
      CodegenModule::RuntimeEntryTag::kRangeError, native_args);
  builder_.CreateUnreachable();
}

llvm::Value* CodegenFunction::EmitGenericCheckBound(
    InstructionInputExtractor I) {
  // TODO(sarkin):
  bool is_known_smi = I.NextInputAsIntPtr();
  auto index = GetValue(I.NextInput());
  auto length = GetValue(I.NextInput());

  auto error_bb = helper_.CreateBasicBlock("bound_error");
  auto smi_check_bb = helper_.CreateBasicBlock("bound_smi_check");
  auto cont_bb = helper_.CreateBasicBlock("bound_cont");

  if (!is_known_smi) {
    auto is_not_smi = helper_.EmitIsNotSmi(index);
    helper_.EmitBrSlowFast(is_not_smi, error_bb, smi_check_bb);
  } else {
    builder_.CreateBr(smi_check_bb);
  }

  builder_.SetInsertPoint(smi_check_bb);
  auto cmp = builder_.CreateICmpULT(index, length);
  helper_.EmitBrFastSlow(cmp, cont_bb, error_bb);

  builder_.SetInsertPoint(error_bb);
  EmitRangeError(index, length);

  builder_.SetInsertPoint(cont_bb);

  return nullptr;
}

llvm::Value* CodegenFunction::EmitStoreStaticField(
    InstructionInputExtractor I) {
  bool needs_write_barrier = I.NextInputAsIntPtr();
  bool can_value_be_smi = I.NextInputAsIntPtr();
  auto field_id = I.NextInputAsIntPtr();
  auto value = GetValue(I.NextInput());

  auto field = helper_.EmitLoadConstant(field_id);
  if (needs_write_barrier) {
    helper_.EmitStoreInObject(field, kFieldStaticValueOffset, value,
                              can_value_be_smi);
  } else {
    helper_.EmitStoreField(field, kFieldStaticValueOffset, value);
  }
  return nullptr;
}

llvm::Value* CodegenFunction::EmitBooleanNegate(InstructionInputExtractor I) {
  auto val = GetValue(I.NextInput());
  return helper_.EmitNegateBool(val);
}

llvm::Value* CodegenFunction::EmitIfThenElse(InstructionInputExtractor I) {
  auto true_val = I.NextInputAsIntPtr();
  auto false_val = I.NextInputAsIntPtr();

  auto cmp = GetValue("v-1");

  auto merge_val = builder_.CreateSelect(cmp, GetConstantInt(true_val),
                                         GetConstantInt(false_val));
  return helper_.EmitSmiTag(merge_val);
}

llvm::Value* CodegenFunction::EmitBoxUint32(InstructionInputExtractor I) {
  auto val = GetValue(I.NextInput());

  assert(val->getType()->isIntegerTy());
  if (val->getType() != IntTy) {
    val = builder_.CreateZExt(val, IntTy);
  }

  return helper_.EmitSmiTag(val);
}

llvm::Value* CodegenFunction::EmitBinaryUint32Op(InstructionInputExtractor I) {
  auto op = I.NextInputAsEnum<TokenKind>();
  auto left = GetValue(I.NextInput());
  auto right = GetValue(I.NextInput());

  // assert(left->getType() == Int32Ty);
  // assert(right->getType() == Int32Ty);
  // TODO(sarkin): Check that op is not division / remainder.
  left = builder_.CreateTrunc(left, Int32Ty);
  right = builder_.CreateTrunc(right, Int32Ty);
  return EmitIntegerArithmetic(op, left, right);
}

llvm::Value* CodegenFunction::EmitUnboxUint32(InstructionInputExtractor I) {
  auto val_cid = I.NextInputAsIntPtr();
  auto val = GetValue(I.NextInput());

  // TODO(sarkin): Only Smis and Mints for now.
  // assert(val_cid == kSmiCid || val_cid == kMintCid);

  if (val_cid == kSmiCid) {
    return helper_.EmitUntagSmi(val);
  } else if (val_cid == kMintCid) {
    return helper_.EmitLoadField(val, kMintValueOffset, Int64Ty);
  } else {
    // TODO(sarkin):
    return helper_.EmitUntagSmi(val);
  }

  return nullptr;
}

llvm::Value* CodegenFunction::EmitUnboxedIntConverter(InstructionInputExtractor I) {
  // TODO(sarkin): Not all conversions implemented.
  auto from = I.NextInputAsEnum<Representation>();
  auto to = I.NextInputAsEnum<Representation>();
  auto val = GetValue(I.NextInput());

  if (from == kUnboxedUint32 && to == kUnboxedInt64) {
    return builder_.CreateZExt(val, Int64Ty);
  } else if (to == kUnboxedUint32 && from == kUnboxedInt64) {
    return builder_.CreateTrunc(val, Int32Ty);
  }
  assert(false);
  return nullptr;
}

llvm::Value* CodegenFunction::EmitShiftInt64Op(InstructionInputExtractor I) {
  // TODO(sarkin): 
  auto op = I.NextInputAsEnum<TokenKind>();
  auto left = GetValue(I.NextInput());
  auto right = GetValue(I.NextInput());

  if (op == kSHL) {
    return builder_.CreateShl(left, right);
  }
  return builder_.CreateAShr(left, right);
}

llvm::Value* CodegenFunction::EmitCheckSmi(InstructionInputExtractor I) {
  // TODO(sarkin): 
  return nullptr;
}

llvm::Value* CodegenFunction::EmitBinarySmiOp(InstructionInputExtractor I) {
  // TODO(sarkin): 
  auto op = I.NextInputAsEnum<TokenKind>();
  auto left = GetValue(I.NextInput());
  auto right = GetValue(I.NextInput());

  left = helper_.EmitSmiToInt(left);
  right = helper_.EmitSmiToInt(right);
  if (op == kBIT_AND) {
    return helper_.EmitNewObject(builder_.CreateAnd(left, right));
  }
  assert(false);
  return nullptr;
}

llvm::Value* CodegenFunction::EmitCid(llvm::Value* val, bool check_smi) {
  auto get_cid = [&](auto val) {
    // Offset in bytes
    const int kCidOffset = 2;
    auto cid = helper_.EmitLoadField(val, kCidOffset, Int16Ty);
    return builder_.CreateZExt(cid, IntTy);
  };

  if (!check_smi) {
    return get_cid(val);
  }

  auto smi_bb = helper_.CreateBasicBlock("smi");
  auto not_smi_bb = helper_.CreateBasicBlock("not_smi");
  auto join_bb = helper_.CreateBasicBlock("join");

  builder_.CreateCondBr(helper_.EmitIsNotSmi(val), not_smi_bb, smi_bb);

  builder_.SetInsertPoint(smi_bb);
  auto cid_smi = GetConstantInt(kSmiCid);
  builder_.CreateBr(join_bb);

  builder_.SetInsertPoint(not_smi_bb);
  auto cid_not_smi = get_cid(val);
  builder_.CreateBr(join_bb);

  builder_.SetInsertPoint(join_bb);
  auto cid = builder_.CreatePHI(IntTy, 2);
  cid->addIncoming(cid_smi, smi_bb);
  cid->addIncoming(cid_not_smi, not_smi_bb);

  return cid;
}

void CodegenFunction::EmitInstruction(const ILInstruction* instr) {
  llvm::Value* produced_value = nullptr;
  switch (instr->tag()) {
#define CASE(Name) case ILInstruction::Tag::k##Name:
#define EMIT(Name)                                                             \
  produced_value = Emit##Name(instr);                                          \
  break;
#define CASE_EMIT(Name, ...) CASE(Name) EMIT(Name)
    FOR_EACH_SUPPORTED_INSTRUCTION(CASE_EMIT)
    default:
      assert(false);
#undef CASE
#undef EMIT
#undef CASE_EMIT
  }
  if (instr->is_value()) {
    assert(produced_value != nullptr);
    AddValue(instr->value_id(), produced_value);
    return;
  }
}

void CodegenFunction::AddValue(std::string value_id, llvm::Value* value) {
  assert(value_id == "v-1" || values_.find(value_id) == values_.end());
  values_[value_id] = value;
}

llvm::Type* CodegenFunction::GetTypeFromRepresentation(
    Representation representation) const {
  // TODO(sarkin): Handle all representations.
  switch (representation) {
    case kTagged:
      return ObjectPtrTy;
    case kUnboxedInt32:
    case kUnboxedUint32:
      return Int32Ty;
    case kUnboxedInt64:
      return Int64Ty;
    case kUnboxedDouble:
      return DoubleTy;
    case kUnboxedFloat32x4:
      return Float32x4Ty;
    case kUnboxedFloat64x2:
      return Float64x2Ty;
    case kUnboxedInt32x4:
      return Int32x4Ty;

    default:
      assert(false);
  }
  return nullptr;
}

llvm::Type* CodegenFunction::GetUnboxedTypeForCid(int cid) const {
  if (cid == kSmiCid || cid == kMintCid) {
    return Int64Ty;
  }
  if (cid == kDoubleCid) {
    return DoubleTy;
  }
  if (cid == kFloat32x4Cid) {
    return Float32x4Ty;
  }
  if (cid == kFloat64x2Cid) {
    return Float64x2Ty;
  }
  assert(false);
  return nullptr;
}

intptr_t CodegenFunction::GetValueOffsetForCid(int cid) const {
  if (cid == kMintCid) {
    return kMintValueOffset;
  }
  if (cid == kDoubleCid) {
    return kDoubleValueOffset;
  }
  if (cid == kFloat32x4Cid) {
    return kFloat32x4ValueOffset;
  }
  if (cid == kFloat64x2Cid) {
    return kFloat64x2ValueOffset;
  }
  assert(false);
  return -1;
}

llvm::Value* CodegenFunction::GetValue(const std::string& value_id) const {
  auto it = values_.find(value_id);
  assert(it != values_.end());
  return it->second;
}

llvm::Value* CodegenFunction::GetValueOrConstant(
    const std::string& value_id) const {
  assert(!value_id.empty());
  if (ILInstruction::IsValue(value_id)) {
    return GetValue(value_id);
  }
  return GetConstantInt(std::stoll(value_id));
}

llvm::BasicBlock* CodegenFunction::GetBlock(intptr_t block_id) const {
  auto it = blocks_.find(block_id);
  assert(it != blocks_.end());
  return it->second;
}

llvm::BasicBlock* CodegenFunction::GetFinalBlock(intptr_t block_id) const {
  auto it = final_blocks_.find(block_id);
  assert(it != final_blocks_.end());
  return it->second;
}

void CodegenFunction::ProcessPhiIncomingValues() {
  for (auto instr : phi_instructions_) {
    llvm::PHINode* phi_node =
        llvm::cast<llvm::PHINode>(GetValue(instr->value_id()));
    const int kEntrySize = 2;
    assert(instr->NumInputs() > 1 + kEntrySize /* 1 for the representation */);
    assert((instr->NumInputs() - 1) % kEntrySize == 0);
    size_t num_income_values = instr->NumInputs() / kEntrySize;
    for (size_t i = 0; i < num_income_values; ++i) {
      auto value = GetValue(instr->InputAt(i * 2 + 1));
      auto block = GetFinalBlock(std::stoull(instr->InputAt((i + 1) * 2)));
      phi_node->addIncoming(value, block);
    }
  }
}

}  // namespace dart_llvm
