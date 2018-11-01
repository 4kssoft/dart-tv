// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "llvm_codegen.h"

#include "il_deserializer.h"

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Utils.h>
#include "llvm/Pass.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"

#include <algorithm>
#include <cassert>

using namespace dart_llvm;

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
        RUNTIME_ENTRY_LIST(DECLARE_RUNTIME_ENTRY_TAG_TO_NAME_ENTRY)
        {RuntimeEntryTag::kNumEntries, "NumEntries"}
    };

#undef DECLARE_RUNTIME_ENTRY_TAG_TO_NAME_ENTRY

CodegenModule::CodegenModule(llvm::Module& module, const DartProgram* program)
    : module_(module), llvm_context_(module.getContext()), program_(program) {
  // Use non-integral pointers for GC-able address space
  // which inhibits incorrect optimizations.
  module_.setDataLayout("ni:1");

  // Used when converting LLVM's Int1Ty to Dart bools by adding
  // the Int1 to the False object's offset in the constant pool.
  assert(program->true_offset() == program->false_offset() + 1);

  Int1Ty = llvm::IntegerType::getInt1Ty(llvm_context_);
  Int8Ty = llvm::IntegerType::getInt8Ty(llvm_context_);
  Int16Ty = llvm::IntegerType::getInt16Ty(llvm_context_);
  Int32Ty = llvm::IntegerType::getInt32Ty(llvm_context_);
  Int64Ty = llvm::IntegerType::getInt64Ty(llvm_context_);
  DoubleTy = llvm::Type::getDoubleTy(llvm_context_);
  VoidTy = llvm::Type::getVoidTy(llvm_context_);

  NonGCObjectPtrTy = llvm::PointerType::getInt8PtrTy(llvm_context_);
  ObjectPtrTy = llvm::PointerType::get(Int8Ty, kGCAddressSpace);
  NonGCPtrToPtrTy = llvm::PointerType::get(ObjectPtrTy, kNonGCAddressSpace);

  ThreadObjectTy = NonGCObjectPtrTy;
  FunctionIDTy = Int64Ty;
  ArgumentArrayTy = NonGCPtrToPtrTy;

  ConstantPoolTy = llvm::ArrayType::get(ObjectPtrTy, program->num_constants());
  constant_pool_ = new llvm::GlobalVariable(
      module_, ConstantPoolTy, false, llvm::GlobalVariable::ExternalLinkage,
      nullptr, kGlobalConstantPoolName);

  FunctionPoolTy =
      llvm::ArrayType::get(NonGCObjectPtrTy, program->NumFunctions());
  function_pool_ = new llvm::GlobalVariable(
      module_, FunctionPoolTy, false, llvm::GlobalVariable::ExternalLinkage,
      nullptr, kGlobalFunctionPoolName);

  FixedParamTrampolineTy = llvm::FunctionType::get(
      ObjectPtrTy, {FunctionIDTy, ThreadObjectTy, ArgumentArrayTy}, false);

  StaticTrampolinesTy =
      llvm::ArrayType::get(NonGCObjectPtrTy, program->NumFunctions());
  static_trampolines_ = new llvm::GlobalVariable(
      module_, StaticTrampolinesTy, false,
      llvm::GlobalVariable::InternalLinkage, nullptr, "_StaticTrampolines");

  NativeArgumentsTy = llvm::StructType::create(
      llvm_context_,
      {ThreadObjectTy, /* argc */ Int64Ty, ArgumentArrayTy, ArgumentArrayTy},
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
}

const llvm::DataLayout& CodegenModule::GetDataLayout() const {
  return module_.getDataLayout();
}

void CodegenModule::GenerateProgram() {
  assert(!already_generated_);
  already_generated_ = true;

  assert(program_ != nullptr);
  // Order matters.
  GenerateRuntimeFunctionDeclarations();
  GenerateSpecialFunctions();
  GenerateFunctions();
  GenerateConstants();

  // TODO(sarkin): Testing some optimizaitons.
  llvm::legacy::FunctionPassManager fpm(&module_);

  llvm::legacy::PassManager mpm;
  // mpm.add(llvm::createFunctionInliningPass());
  mpm.add(llvm::createStripSymbolsPass());
  mpm.run(module_);

  fpm.add(llvm::createPromoteMemoryToRegisterPass());
  fpm.add(llvm::createCFGSimplificationPass());
  fpm.add(llvm::createConstantPropagationPass());
  fpm.add(llvm::createLICMPass());
  fpm.add(llvm::createEarlyCSEPass());
  fpm.add(llvm::createGVNPass());
  fpm.add(llvm::createAggressiveDCEPass());
  fpm.add(llvm::createSinkingPass());

  for (auto& f : module_) {
    fpm.run(f);
  }
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
    ty = Int64Ty;
  }
  return llvm::ConstantInt::get(ty, val);
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

  auto unbox_ty = llvm::FunctionType::get(Int64Ty, {ObjectPtrTy}, false);
  auto box_ty = llvm::FunctionType::get(ObjectPtrTy, {Int64Ty}, false);

  box_ = llvm::Function::Create(box_ty, llvm::GlobalVariable::ExternalLinkage,
                                "LLVMBoxInt64", module_);
  unbox_ =
      llvm::Function::Create(unbox_ty, llvm::GlobalVariable::ExternalLinkage,
                             "LLVMUnboxInt64", module_);
}

void CodegenModule::GenerateSpecialFunctions() {
  GenerateDartToLLVMTrampoline();
  GenerateFixedParamsTrampolines();
  GenerateLLVMToRuntimeTrampoline();
}

void CodegenModule::GenerateDartToLLVMTrampoline() {
  std::vector<llvm::Type*> params{FunctionIDTy, ThreadObjectTy,
                                  ArgumentArrayTy};
  llvm::Function* trampoline = llvm::Function::Create(
      FixedParamTrampolineTy, llvm::GlobalVariable::ExternalLinkage,
      kDartToLLVMTrampolineName, module_);

  auto builder = std::make_unique<llvm::IRBuilder<>>(llvm_context_);
  auto bb = llvm::BasicBlock::Create(llvm_context_, "entry", trampoline);
  builder->SetInsertPoint(bb);

  llvm::Value* func_id = trampoline->arg_begin();
  llvm::Value* thr = trampoline->arg_begin() + 1;
  llvm::Value* args = trampoline->arg_begin() + 2;

  llvm::Value* callee = nullptr;
  {
    std::vector<llvm::Value*> idx{GetConstantInt(/* GEP specific */ 0),
                                  func_id};
    callee = builder->CreateLoad(
        builder->CreateGEP(StaticTrampolinesTy, static_trampolines_, idx));
    callee = builder->CreateBitCast(
        callee,
        llvm::PointerType::get(FixedParamTrampolineTy, kNonGCAddressSpace));
  }

  builder->CreateRet(builder->CreateCall(FixedParamTrampolineTy, callee,
                                         {func_id, thr, args}));
}

void CodegenModule::GenerateLLVMToRuntimeTrampoline() {
  // TODO(sarkin):
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
  // NativeArguments passed by value.
  trampoline->addParamAttr(kNativeArgumentsOffset,
                           llvm::Attribute::AttrKind::ByVal);

  // The following instructions are added as prologue data:
  // mov r10, rsp
  // sub r10, 8
  // mov [rdi + rsi], r10
  // Where rdi is the thread object and rsi is the exit_frame_info offset in the thread object.
  auto instructions_ty = llvm::ArrayType::get(Int32Ty, 3);
  std::array<llvm::Constant*, 3> instructions{
      GetConstantInt(0x49e28949, Int32Ty), GetConstantInt(0x4c08ea83, Int32Ty),
      GetConstantInt(0x90371489, Int32Ty)};
  trampoline->setPrologueData(
      llvm::ConstantArray::get(instructions_ty, instructions));

  auto builder = std::make_unique<llvm::IRBuilder<>>(llvm_context_);
  auto bb = llvm::BasicBlock::Create(llvm_context_, "entry", trampoline);
  builder->SetInsertPoint(bb);

  llvm::Value* func_id = trampoline->arg_begin() + kFunctionIDOffset;
  llvm::Value* native_args = trampoline->arg_begin() + kNativeArgumentsOffset;
  llvm::Value* callee = nullptr;
  {
    std::vector<llvm::Value*> idx{GetConstantInt(/* GEP specific */ 0),
                                  func_id};
    callee = builder->CreateLoad(
        builder->CreateGEP(RuntimeFunctionsTy, runtime_functions_, idx));
    callee = builder->CreateBitCast(
        callee, llvm::PointerType::get(RuntimeFunctionTy, 0));
  }
  auto call = builder->CreateCall(RuntimeFunctionTy, callee, {native_args});
  // Pass NativeArguments by value.
  call->addParamAttr(0, llvm::Attribute::ByVal);
  builder->CreateRetVoid();

  llvm_to_runtime_trampoline_ = trampoline;
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

llvm::Function* CodegenModule::GetOrCreateFixedParamTrampoline(
    size_t num_params) {
  auto it = fixed_params_trampolines_.find(num_params);
  if (it != fixed_params_trampolines_.end()) {
    return it->second;
  }
  auto trampoline = llvm::Function::Create(
      FixedParamTrampolineTy, llvm::GlobalVariable::InternalLinkage,
      "_FixedParamTrampoline#" + std::to_string(num_params), module_);

  auto builder = std::make_unique<llvm::IRBuilder<>>(llvm_context_);
  auto bb = llvm::BasicBlock::Create(llvm_context_, "entry", trampoline);
  builder->SetInsertPoint(bb);

  llvm::Value* func_id = trampoline->arg_begin();
  llvm::Value* thr = trampoline->arg_begin() + 1;
  llvm::Value* args = trampoline->arg_begin() + 2;

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
    auto argi = builder->CreateLoad(builder->CreateGEP(ObjectPtrTy, args, idx));
    extracted_args.push_back(argi);
  }

  llvm::Value* callee = nullptr;
  auto callee_ty = GetStaticFunctionType(num_params);
  {
    std::vector<llvm::Value*> idx{GetConstantInt(/* GEP specific */ 0),
                                  func_id};
    callee = builder->CreateLoad(
        builder->CreateGEP(FunctionPoolTy, function_pool_, idx));
    callee = builder->CreateBitCast(
        callee, llvm::PointerType::get(callee_ty, kNonGCAddressSpace));
  }
  builder->CreateRet(builder->CreateCall(callee_ty, callee, extracted_args));

  fixed_params_trampolines_.emplace(num_params, trampoline);
  return trampoline;
}

void CodegenModule::GenerateFunctions() {
  // TODO(sarkin): Only static functions for now
  std::vector<llvm::Constant*> function_pool_entries(program_->NumFunctions());
  for (size_t i = 0; i < program_->NumFunctions(); ++i) {
    auto func = program_->FunctionAt(i);

    if (func->is_static()) {
      auto llvm_func = llvm::Function::Create(
          GetStaticFunctionType(func->num_params()),
          llvm::GlobalVariable::ExternalLinkage, func->name(), module_);

      function_pool_entries[func->id()] =
          llvm::ConstantExpr::getBitCast(llvm_func, NonGCObjectPtrTy);

      CodegenFunction(*this, func, llvm_func).GenerateCode();
    } else {
      assert(false);
    }
  }
  function_pool_->setInitializer(
      llvm::ConstantArray::get(FunctionPoolTy, function_pool_entries));
}

void CodegenModule::GenerateConstants() {
  std::vector<llvm::Constant*> constant_pool_entries(
      program_->num_constants(), llvm::ConstantPointerNull::get(ObjectPtrTy));
  constant_pool_->setInitializer(
      llvm::ConstantArray::get(ConstantPoolTy, constant_pool_entries));
}

const std::string CodegenModule::kGlobalConstantPoolName =
    "_GlobalConstantPool";
const std::string CodegenModule::kGlobalFunctionPoolName =
    "_GlobalFunctionPool";
const std::string CodegenModule::kDartToLLVMTrampolineName =
    "_DartToLLVMTrampoline";

CodegenFunction::InstructionInputExtractor::InstructionInputExtractor(
    const ILInstruction* instr)
    : instr_(instr) {}

std::string CodegenFunction::InstructionInputExtractor::NextInput() {
  return instr_->InputAt(idx_++);
}

intptr_t CodegenFunction::InstructionInputExtractor::NextInputAsIntPtr() {
  return stoll(NextInput());
}

int64_t CodegenFunction::InstructionInputExtractor::NextInputAsInt64() {
  return stoll(NextInput());
}

template <typename T>
T CodegenFunction::InstructionInputExtractor::NextInputAsEnum() {
  return static_cast<T>(NextInputAsIntPtr());
}

size_t CodegenFunction::InstructionInputExtractor::NumInputs() const {
  return instr_->NumInputs();
}

CodegenFunction::CodegenFunction(CodegenModule& cgm,
                                 const DartFunction* func,
                                 llvm::Function* llvm_func)
    : CodegenTypeCache(cgm), cgm_(cgm), func_(func), llvm_func_(llvm_func) {
  builder_ = std::make_unique<llvm::IRBuilder<>>(cgm_.GetLLVMContext());
}

void CodegenFunction::GenerateCode() {
  assert(!already_generated_);
  already_generated_ = true;

  for (size_t i = 0; i < func_->NumBasicBlocks(); ++i) {
    auto bb = func_->BasicBlockAt(i);
    blocks_[bb->id()] = CreateBasicBlock(bb->id());
  }

  for (size_t i = 0; i < func_->NumBasicBlocks(); ++i) {
    auto bb = func_->BasicBlockAt(i);
    GenerateBasicBlock(bb, blocks_[bb->id()]);
  }

  // Assume there's one function entry block and jump into it from the entry block.
  // Furthermore, assume 0 is the entry and 1 is the only function entry block,
  // which *should* be the case since blocks are in reverse postorder.
  const int kEntryBlock = 0;
  const int kFunctionEntryBlock = 1;
  builder_->SetInsertPoint(blocks_[kEntryBlock]);
  builder_->CreateBr(blocks_[kFunctionEntryBlock]);

  ProcessPhiIncomingValues();
}

llvm::BasicBlock* CodegenFunction::CreateBasicBlock(intptr_t id) {
  return llvm::BasicBlock::Create(cgm_.GetLLVMContext(), std::to_string(id),
                                  llvm_func_);
}

llvm::BasicBlock* CodegenFunction::CreateBasicBlock(std::string id) {
  return llvm::BasicBlock::Create(cgm_.GetLLVMContext(), id, llvm_func_);
}

void CodegenFunction::GenerateBasicBlock(const DartBasicBlock* bb,
                                         llvm::BasicBlock* llvm_bb) {
  builder_->SetInsertPoint(llvm_bb);

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
  auto const_id = I.NextInputAsIntPtr();
  std::vector<llvm::Value*> idx{GetConstantInt(/* GEP specific */ 0),
                                GetConstantInt(const_id)};

  return builder_->CreateLoad(
      builder_->CreateGEP(ConstantPoolTy, cgm_.constant_pool(), idx));
}

llvm::Value* CodegenFunction::EmitReturn(InstructionInputExtractor I) {
  auto ret_val = I.NextInput();
  builder_->CreateRet(GetValue(ret_val));
  return nullptr;
}

llvm::Value* CodegenFunction::EmitGoto(InstructionInputExtractor I) {
  auto dest = I.NextInputAsIntPtr();
  builder_->CreateBr(GetBlock(dest));
  return nullptr;
}

llvm::Value* CodegenFunction::EmitParameter(InstructionInputExtractor I) {
  auto param_idx = I.NextInputAsIntPtr();
  return llvm_func_->arg_begin() + (param_idx + kDartParamOffset);
}

llvm::Value* CodegenFunction::EmitPhi(InstructionInputExtractor I) {
  auto representation = I.NextInputAsEnum<Representation>();
  auto phi_ty = GetTypeFromRepresentation(representation);
  size_t num_income_values = I.NumInputs();
  assert(num_income_values > 1);
  // For processing later as needed values might not be emitted yet.
  phi_instructions_.push_back(I.instr());
  return builder_->CreatePHI(phi_ty, num_income_values);
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

llvm::Value* CodegenFunction::EmitBinaryInt64Op(InstructionInputExtractor I) {
  auto op = I.NextInputAsEnum<TokenKind>();
  auto left = GetValue(I.NextInput());
  auto right = GetValue(I.NextInput());

  assert(left->getType() == Int64Ty);
  assert(right->getType() == Int64Ty);

  const std::unordered_map<TokenKind, llvm::Instruction::BinaryOps> kDartOpToLLVM{
      {TokenKind::kADD, llvm::Instruction::BinaryOps::Add},
      {TokenKind::kSUB, llvm::Instruction::BinaryOps::Sub},
      {TokenKind::kMUL, llvm::Instruction::BinaryOps::Mul},
      {TokenKind::kBIT_AND, llvm::Instruction::BinaryOps::And},
      {TokenKind::kBIT_OR, llvm::Instruction::BinaryOps::Or},
      {TokenKind::kBIT_XOR, llvm::Instruction::BinaryOps::Xor}};
  auto llvm_op = kDartOpToLLVM.find(op);
  assert(llvm_op != kDartOpToLLVM.end());

  return builder_->CreateBinOp(llvm_op->second, left, right);
}

llvm::Value* CodegenFunction::EmitRelationalOpInt64(TokenKind op,
                                                    RelationalOpCid op_type,
                                                    llvm::Value* left,
                                                    llvm::Value* right) {
  // TODO(sarkin): Handle the case of tagged Smi, which have type ObjPtrTy
  if (left->getType()->isPointerTy()) {
    assert(right->getType()->isPointerTy());
    assert(false);
  }
  const std::unordered_map<TokenKind, llvm::CmpInst::Predicate> kDartOpToLLVM{
      {TokenKind::kLT, llvm::CmpInst::Predicate::ICMP_SLT},
      {TokenKind::kGT, llvm::CmpInst::Predicate::ICMP_SGT},
      {TokenKind::kGTE, llvm::CmpInst::Predicate::ICMP_SGE},
      {TokenKind::kLTE, llvm::CmpInst::Predicate::ICMP_SLE},
      {TokenKind::kEQ, llvm::CmpInst::Predicate::ICMP_EQ},
      {TokenKind::kNE, llvm::CmpInst::Predicate::ICMP_NE}};

  auto llvm_op = kDartOpToLLVM.find(op);
  assert(llvm_op != kDartOpToLLVM.end());
  return builder_->CreateICmp(llvm_op->second, left, right);
}

llvm::Value* CodegenFunction::EmitRelationalOpDouble(TokenKind op,
                                                     RelationalOpCid op_type,
                                                     llvm::Value* left,
                                                     llvm::Value* right) {
  // Using ordered comparisons, i.e. having NaN as either operand results in false
  assert(left->getType()->isDoubleTy());
  assert(right->getType()->isDoubleTy());

  const std::unordered_map<TokenKind, llvm::CmpInst::Predicate> kDartOpToLLVM{
      {TokenKind::kLT, llvm::CmpInst::Predicate::FCMP_OLT},
      {TokenKind::kGT, llvm::CmpInst::Predicate::FCMP_OGT},
      {TokenKind::kGTE, llvm::CmpInst::Predicate::FCMP_OGE},
      {TokenKind::kLTE, llvm::CmpInst::Predicate::FCMP_OLE},
      {TokenKind::kEQ, llvm::CmpInst::Predicate::FCMP_OEQ},
      {TokenKind::kNE, llvm::CmpInst::Predicate::FCMP_ONE}};

  auto llvm_op = kDartOpToLLVM.find(op);
  assert(llvm_op != kDartOpToLLVM.end());
  return builder_->CreateICmp(llvm_op->second, left, right);
}

llvm::Value* CodegenFunction::EmitBool(llvm::Value* value) {
  // TODO(sarkin): Should this be somehow optimized?
  assert(value != nullptr);
  assert(value->getType() == Int1Ty);

  value = builder_->CreateZExt(value, Int64Ty);
  auto offset = builder_->CreateAdd(value, GetConstantInt(cgm_.FalseOffset()));
  return builder_->CreateLoad(builder_->CreateGEP(
      ConstantPoolTy, cgm_.constant_pool(), {GetConstantInt(0), offset}));
}

llvm::Value* CodegenFunction::EmitNull() {
  auto offset = GetConstantInt(cgm_.NullOffset());
  return builder_->CreateLoad(builder_->CreateGEP(
      ConstantPoolTy, cgm_.constant_pool(), {GetConstantInt(0), offset}));
}

llvm::Value* CodegenFunction::EmitRelationalOp(InstructionInputExtractor I) {
  auto op = I.NextInputAsEnum<TokenKind>();
  auto op_type = I.NextInputAsEnum<RelationalOpCid>();
  auto left = GetValue(I.NextInput());
  auto right = GetValue(I.NextInput());
  llvm::Value* result = nullptr;
  switch (op_type) {
    case RelationalOpCid::kInt64:
      result = EmitRelationalOpInt64(op, op_type, left, right);
      break;
    case RelationalOpCid::kDouble:
      result = EmitRelationalOpDouble(op, op_type, left, right);
      break;
    default:
      assert(false);
  }
  if (!I.instr()->IsComparisonInBranch()) {
    result = EmitBool(result);
  }
  return result;
}

llvm::Value* CodegenFunction::EmitBranch(InstructionInputExtractor I) {
  // The -1 comes from the ComparisonInstr, which has a ssa_tmp_index -1 and
  // is processed right before the branch instruction
  auto comp_result = GetValue("v-1");
  auto true_block = GetBlock(I.NextInputAsIntPtr());
  auto false_block = GetBlock(I.NextInputAsIntPtr());
  builder_->CreateCondBr(comp_result, true_block, false_block);
  return nullptr;
}

// TODO(sarkin):
llvm::Value* CodegenFunction::EmitBoxInt64(InstructionInputExtractor I) {
  auto val = GetValue(I.NextInput());
  return builder_->CreateCall(cgm_.box_, {val});
}

llvm::Value* CodegenFunction::EmitUnboxInt64(InstructionInputExtractor I) {
  auto val = GetValue(I.NextInput());
  return builder_->CreateCall(cgm_.unbox_, {val});
}

llvm::Value* CodegenFunction::EmitCheckStackOverflow(
    InstructionInputExtractor I) {
  // TODO(sarkin):
  return nullptr;
}

llvm::Value* CodegenFunction::EmitNativeArguments(intptr_t argc_tag,
                                                  llvm::Value* argv,
                                                  llvm::Value* ret_val) {
  // TODO(sarkin): Refactor field access? This is ugly.
  auto native_args = builder_->CreateAlloca(NativeArgumentsTy);
  {
    std::vector<llvm::Value*> idx{GetConstantInt(/* GEP specific */ 0, Int32Ty),
                                  GetConstantInt(0, Int32Ty)};
    auto thread_field =
        builder_->CreateGEP(NativeArgumentsTy, native_args, idx);
    builder_->CreateStore(GetThread(), thread_field);
  }

  {
    std::vector<llvm::Value*> idx{GetConstantInt(/* GEP specific */ 0, Int32Ty),
                                  GetConstantInt(1, Int32Ty)};
    auto argc_field = builder_->CreateGEP(NativeArgumentsTy, native_args, idx);
    builder_->CreateStore(GetConstantInt(argc_tag), argc_field);
  }

  if (argv != nullptr) {
    std::vector<llvm::Value*> idx{GetConstantInt(/* GEP specific */ 0, Int32Ty),
                                  GetConstantInt(2, Int32Ty)};
    auto argv_field = builder_->CreateGEP(NativeArgumentsTy, native_args, idx);
    builder_->CreateStore(argv, argv_field);
  }
  if (ret_val != nullptr) {
    std::vector<llvm::Value*> idx{GetConstantInt(/* GEP specific */ 0, Int32Ty),
                                  GetConstantInt(3, Int32Ty)};
    auto ret_val_field =
        builder_->CreateGEP(NativeArgumentsTy, native_args, idx);
    builder_->CreateStore(ret_val, ret_val_field);
  }

  return native_args;
}

void CodegenFunction::EmitCallToRuntimeTrampoline(
    CodegenModule::RuntimeEntryTag tag,
    llvm::Value* native_args) {
  auto call = builder_->CreateCall(
      cgm_.llvm_to_runtime_trampoline(),
      {GetThread(), GetConstantInt(cgm_.TopExitFrameInfoOffset()),
       GetConstantInt(tag), native_args});

  // Pass NativeArguments by value.
  const int kNativeArgumentsOffset = 3;
  call->addParamAttr(kNativeArgumentsOffset, llvm::Attribute::ByVal);
}

void CodegenFunction::EmitNullError() {
  auto native_args = EmitNativeArguments(0, nullptr, nullptr);
  EmitCallToRuntimeTrampoline(CodegenModule::RuntimeEntryTag::kNullError,
                              native_args);
}

llvm::Value* CodegenFunction::EmitCheckNull(InstructionInputExtractor I) {
  // TODO(sarkin):
  auto val = GetValue(I.NextInput());
  auto error_bb = CreateBasicBlock("null_error");
  auto cont_bb = CreateBasicBlock("null_cont");

  auto null = EmitNull();

  auto cmp = builder_->CreateICmpEQ(val, null);
  builder_->CreateCondBr(cmp, error_bb, cont_bb);

  builder_->SetInsertPoint(error_bb);
  EmitNullError();
  builder_->CreateBr(cont_bb);

  builder_->SetInsertPoint(cont_bb);

  return nullptr;
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
  assert(produced_value == nullptr);
}

void CodegenFunction::AddValue(std::string value_id, llvm::Value* value) {
  assert(values_.find(value_id) == values_.end());
  values_[value_id] = value;
  llvm_value_to_block_[value] = GetCurrentLLVMBasicBlock();
}

llvm::BasicBlock* CodegenFunction::GetCurrentLLVMBasicBlock() const {
  auto bb = builder_->GetInsertBlock();
  assert(bb != nullptr);
  return bb;
}

llvm::Value* CodegenFunction::GetThread() const {
  // Thread is always the first parameter.
  assert(llvm_func_ != nullptr);
  return llvm_func_->arg_begin();
}

llvm::Type* CodegenFunction::GetTypeFromRepresentation(
    Representation representation) const {
  // TODO(sarkin): Handle all representations.
  switch (representation) {
    // TODO(sarkin): Might want to give them different types.
    case kTagged:
    case kUntagged:
      return ObjectPtrTy;
    case kUnboxedInt64:
      return Int64Ty;
    case kUnboxedDouble:
      return DoubleTy;
    default:
      assert(false);
  }
  return nullptr;
}

llvm::Value* CodegenFunction::GetValue(std::string value_id) const {
  auto it = values_.find(value_id);
  assert(it != values_.end());
  return it->second;
}

llvm::BasicBlock* CodegenFunction::GetBlock(intptr_t block_id) const {
  auto it = blocks_.find(block_id);
  assert(it != blocks_.end());
  return it->second;
}

llvm::Constant* CodegenFunction::GetConstantInt(int64_t val,
                                                llvm::IntegerType* ty) const {
  if (ty == nullptr) {
    ty = Int64Ty;
  }
  return llvm::ConstantInt::get(ty, val);
}

void CodegenFunction::ProcessPhiIncomingValues() {
  for (auto instr : phi_instructions_) {
    llvm::PHINode* phi_node =
        llvm::cast<llvm::PHINode>(GetValue(instr->value_id()));
    size_t num_income_values = instr->NumInputs();
    assert(num_income_values > 1);
    num_income_values--;
    for (size_t i = 0; i < num_income_values; ++i) {
      auto value = GetValue(instr->InputAt(i + 1));
      auto block = llvm_value_to_block_[value];
      phi_node->addIncoming(value, block);
    }
  }
}
