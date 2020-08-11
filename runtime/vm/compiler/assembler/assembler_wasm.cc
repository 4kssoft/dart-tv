// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/compiler/assembler/assembler_wasm.h"

// In a few cases module_builder() will not be available, or will not be safe
// to call just yet (i.e. in a constructor initializer list), so care has to
// be taken when using this macro.
#define Z (module_builder()->zone())

namespace wasm {
namespace {
using ::dart::OS;
using ::dart::SExpList;
using ::dart::SExpression;
using ::dart::SExpSymbol;
using ::dart::Thread;
}  // namespace

SExpression* wasm::PrimitiveType::Serialize() {
  switch (type_) {
    case ValType::kI32:
      return new (Z) SExpSymbol("i32");
    case ValType::kI64:
      return new (Z) SExpSymbol("i64");
    case ValType::kF32:
      return new (Z) SExpSymbol("f32");
    case ValType::kF64:
      return new (Z) SExpSymbol("f64");
    default:
      UNREACHABLE();
  }
}

WasmModuleBuilder* Instruction::module_builder() const {
  return block_->module_builder();
}

SExpression* LocalGet::Serialize() {
  return new (Z) SExpSymbol(OS::SCreate(Z, "local.get $%s", local_->name()));
}

SExpression* LocalSet::Serialize() {
  return new (Z) SExpSymbol(OS::SCreate(Z, "local.set $%s", local_->name()));
}

SExpression* Int32Add::Serialize() {
  return new (Z) SExpSymbol("i32.add");
}

SExpression* Constant::Serialize() {
  return new (Z) SExpSymbol(OS::SCreate(Z, "i32.const %" Pu32, value_));
}

// Serializing a Goto doesn't produce Wasm-compliant code, but should be
// helpful for testing.
SExpression* Goto::Serialize() {
  return new (Z)
      SExpSymbol(OS::SCreate(Z, "goto B%" Pd, target_block_->block_id()));
}

If::If(BasicBlock* basic_block)
    : Instruction(basic_block),
      test_(new (Z) InstructionList(block())),
      then_(new (Z) InstructionList(block())),
      otherwise_(new (Z) InstructionList(block())) {}

// TODO(andreicostin): Add and serialize result type.
SExpression* If::Serialize() {
  // Serialize test condition.
  auto sexp_test = new (Z) SExpList(Z);
  sexp_test->Add(test_->Serialize());

  // Serialize then branch.
  auto sexp_then = new (Z) SExpList(Z);
  sexp_then->Add(new SExpSymbol("then"));
  sexp_then->Add(then_->Serialize());

  // Serialize otherwise branch.
  auto sexp_otherwise = new (Z) SExpList(Z);
  sexp_then->Add(new SExpSymbol("else"));
  sexp_then->Add(otherwise_->Serialize());

  // Produce final SExpression.
  auto sexp = new (Z) SExpList(Z);
  sexp->Add(new SExpSymbol("if"));
  sexp->Add(sexp_test);
  sexp->Add(sexp_then);
  sexp->Add(sexp_otherwise);
  return sexp;
}

WasmModuleBuilder* Local::module_builder() const {
  return function_->module_builder();
}

SExpression* Local::Serialize() {
  auto sexp = new (Z) SExpList(Z);
  switch (kind_) {
    case LocalKind::kLocal:
      sexp->Add(new (Z) SExpSymbol("local"));
      break;
    case LocalKind::kParam:
      sexp->Add(new (Z) SExpSymbol("param"));
      break;
    default:
      UNREACHABLE();
  }
  if (strcmp(name_, "") != 0) {
    sexp->Add(new (Z) SExpSymbol(OS::SCreate(Z, "$%s", name_)));
  }
  sexp->Add(type_->Serialize());
  return sexp;
}

InstructionList::InstructionList(BasicBlock* block)
    : block_(block), instructions_(Z, 16) {}

WasmModuleBuilder* InstructionList::module_builder() const {
  return block_->module_builder();
}

SExpression* InstructionList::Serialize() {
  auto sexp = new (Z) SExpList(Z);
  for (Instruction* instr : instructions_) {
    sexp->Add(instr->Serialize());
  }
  return sexp;
}

Instruction* InstructionList::AddLocalGet(Local* local) {
  instructions_.Add(new (Z) LocalGet(block_, local));
  return instructions_.Last();
}

Instruction* InstructionList::AddLocalSet(Local* local) {
  instructions_.Add(new (Z) LocalSet(block_, local));
  return instructions_.Last();
}

Instruction* InstructionList::AddInt32Add() {
  instructions_.Add(new (Z) Int32Add(block_));
  return instructions_.Last();
}

Instruction* InstructionList::AddConstant(uint32_t value) {
  instructions_.Add(new (Z) Constant(block_, value));
  return instructions_.Last();
}

Instruction* InstructionList::AddGoto(BasicBlock* target_block) {
  instructions_.Add(new (Z) Goto(block_, target_block));
  return instructions_.Last();
}

Instruction* InstructionList::AddIf() {
  instructions_.Add(new (Z) If(block_));
  return instructions_.Last();
}

BasicBlock::BasicBlock(Function* function, intptr_t block_id)
    : function_(function),
      block_id_(block_id),
      instructions_(new (Z) InstructionList(this)) {}

WasmModuleBuilder* BasicBlock ::module_builder() const {
  return function_->module_builder();
}

SExpression* BasicBlock::Serialize() {
  auto sexp = new (Z) SExpList(Z);
  sexp->Add(new (Z) SExpSymbol(OS::SCreate(Z, "B%" Pd, block_id_)));
  sexp->Add(instructions_->Serialize());
  return sexp;
}

Function::Function(WasmModuleBuilder* module_builder,
                   const char* name,
                   uint32_t index,
                   Type* result_type)
    : module_builder_(module_builder),
      name_(name),
      index_(index),
      result_type_(result_type),
      locals_(module_builder->zone(), 16),
      body_(module_builder->zone(), 16) {}

SExpression* Function::Serialize() {
  auto sexp = new (Z) SExpList(Z);
  sexp->Add(new (Z) SExpSymbol("func"));
  if (strcmp(name_, "") != 0) {
    sexp->Add(new (Z) SExpSymbol(OS::SCreate(Z, "$%s", name_)));
  }
  // Serialize parameters.
  for (Local* it : locals_) {
    if (it->kind() == Local::LocalKind::kParam) {
      sexp->Add(it->Serialize());
    }
  }
  // Serialize the result type.
  if (result_type_ != nullptr) {
    auto sexp_result = new (Z) SExpList(Z);
    // TODO(andreicostin): Introduce the concept of ResultType,
    // like in the Wasm specification.
    sexp_result->Add(new (Z) SExpSymbol("result"));
    sexp_result->Add(result_type_->Serialize());
    sexp->Add(sexp_result);
  }
  // Serialize locals.
  for (Local* it : locals_) {
    if (it->kind() == Local::LocalKind::kLocal) {
      sexp->Add(it->Serialize());
    }
  }
  // Serialize the body.
  // TODO(andreicostin): This should be changed in the case when there
  // is only one basic block to not output the "blocks" statement.
  // This will always be the case after Function::Linearize() has been
  // called on the function.
  auto sexp_body = new (Z) SExpList(Z);
  sexp_body->Add(new (Z) SExpSymbol("blocks"));
  for (BasicBlock* it : body_) {
    sexp_body->Add(it->Serialize());
  }
  sexp->Add(sexp_body);
  return sexp;
}

Local* Function::AddLocal(Local::LocalKind kind, Type* type, const char* name) {
  // No further params can be declared after the first
  // local in a Wasm function header.
  ASSERT(kind == Local::LocalKind::kLocal || locals_.is_empty() ||
         locals_.Last()->kind() == Local::LocalKind::kParam);
  locals_.Add(new (Z) Local(this, kind, type, name, locals_.length()));
  return locals_.Last();
}

BasicBlock* Function::AddBlock(intptr_t block_id) {
  body_.Add(new (Z) BasicBlock(this, block_id));
  return body_.Last();
}

Local* Function::GetLocalByName(const char* name) {
  for (Local* it : locals_) {
    if (strcmp(it->name(), name) == 0) {
      return it;
    }
  }
  FATAL("Local variable by name lookup returned no matching result.");
}

Local* Function::GetLocalByIndex(int id) {
  ASSERT(0 <= id && id < locals_.length());
  return locals_[id];
}

BasicBlock* Function::GetBlockById(intptr_t block_id) {
  for (BasicBlock* block : body_) {
    if (block->block_id() == block_id) {
      return block;
    }
  }
  FATAL("Basic block lookup by id returned no matching result.");
}

WasmModuleBuilder::WasmModuleBuilder(Thread* thread)
    : i32_(this, PrimitiveType::ValType::kI32),
      i64_(this, PrimitiveType::ValType::kI64),
      f32_(this, PrimitiveType::ValType::kF32),
      f64_(this, PrimitiveType::ValType::kF64),
      thread_(thread),
      zone_(thread->zone()),
      functions_(thread->zone(), 16) {}

SExpression* WasmModuleBuilder::Serialize() {
  auto sexp = new (zone()) SExpList(zone());
  sexp->Add(new (zone()) SExpSymbol("module"));
  for (Function* it : functions_) {
    sexp->Add(it->Serialize());
  }
  return sexp;
}

Function* WasmModuleBuilder::AddFunction(const char* name, Type* result_type) {
  functions_.Add(new (zone())
                     Function(this, name, functions_.length(), result_type));
  return functions_.Last();
}

// Collapses the basic block structure of a function into a
// "loop-label-switch" construct.
void Function::Linearize() {
  // If there is only one block, then there is no need for simplification,
  // (unless it may also jump back to itself).
  if (body_.length() <= 1) {
    return;
  }
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

}  // namespace wasm
