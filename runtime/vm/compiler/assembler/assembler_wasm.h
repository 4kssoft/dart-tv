// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef RUNTIME_VM_COMPILER_ASSEMBLER_ASSEMBLER_WASM_H_
#define RUNTIME_VM_COMPILER_ASSEMBLER_ASSEMBLER_WASM_H_

#include "vm/compiler/backend/sexpression.h"
#include "vm/thread.h"

namespace wasm {

// Forward declarations.
#define FOR_ALL_WASM_CONSTRUCTS(M)                                             \
  M(Type)                                                                      \
  M(PrimitiveType)                                                             \
  M(Instruction)                                                               \
  M(LocalGet)                                                                  \
  M(LocalSet)                                                                  \
  M(Int32Add)                                                                  \
  M(Constant)                                                                  \
  M(Goto)                                                                      \
  M(If)                                                                        \
  M(Local)                                                                     \
  M(InstructionList)                                                           \
  M(BasicBlock)                                                                \
  M(Function)                                                                  \
  M(WasmModuleBuilder)

#define FORWARD_DECLARATION(type) class type;
FOR_ALL_WASM_CONSTRUCTS(FORWARD_DECLARATION)
#undef FORWARD_DECLARATION

// Abstract base class for Wasm types.
class Type : public dart::ZoneAllocated {
 public:
  virtual dart::SExpression* Serialize() = 0;
  virtual ~Type() = default;
  WasmModuleBuilder* module_builder() { return module_builder_; }

 protected:
  explicit Type(WasmModuleBuilder* module_builder)
      : module_builder_(module_builder) {}

 private:
  WasmModuleBuilder* const module_builder_;

  DISALLOW_COPY_AND_ASSIGN(Type);
};

// Class for Wasm primitive types: integers and floting points.
class PrimitiveType : public Type {
 public:
  dart::SExpression* Serialize() override;

 private:
  enum class ValType { kI32, kI64, kF32, kF64 };

  PrimitiveType(WasmModuleBuilder* module_builder, ValType type)
      : Type(module_builder), type_(type) {}

  const ValType type_;

  friend class WasmModuleBuilder;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(PrimitiveType);
};

// Abstract base class for Wasm instructions.
class Instruction : public dart::ZoneAllocated {
 public:
  virtual dart::SExpression* Serialize() = 0;
  virtual ~Instruction() = default;

  WasmModuleBuilder* module_builder() const;
  BasicBlock* block() const { return block_; }

 protected:
  explicit Instruction(BasicBlock* block) : block_(block) {}

 private:
  BasicBlock* const block_;

  DISALLOW_COPY_AND_ASSIGN(Instruction);
};

// Wasm local.get instruction.
class LocalGet : public Instruction {
 public:
  dart::SExpression* Serialize() override;

 private:
  LocalGet(BasicBlock* block, Local* local)
      : Instruction(block), local_(local) {}

  Local* const local_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(LocalGet);
};

// Wasm local.set instruction.
class LocalSet : public Instruction {
 public:
  dart::SExpression* Serialize() override;

 private:
  LocalSet(BasicBlock* block, Local* local)
      : Instruction(block), local_(local) {}

  Local* const local_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(LocalSet);
};

// Wasm int32.add instruction.
// TODO(andreicostin): Generalize this for other operations and types.
class Int32Add : public Instruction {
 public:
  dart::SExpression* Serialize() override;

 private:
  explicit Int32Add(BasicBlock* block) : Instruction(block) {}

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Int32Add);
};

// Wasm i32.const instruction.
// TODO(andreicostin): Generalize this for other types.
class Constant : public Instruction {
 public:
  dart::SExpression* Serialize() override;

 private:
  Constant(BasicBlock* block, uint32_t value)
      : Instruction(block), value_(value) {}

  uint32_t value_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Constant);
};

// Synthetic Wasm goto/jump instruction.
// While not available in Wasm due to its structured control flow, it is used
// in an intermediary representation. Serializing it does not produce
// Wasm-compliant code.
class Goto : public Instruction {
 public:
  dart::SExpression* Serialize() override;

 private:
  Goto(BasicBlock* block, BasicBlock* target_block)
      : Instruction(block), target_block_(target_block) {}

  BasicBlock* const target_block_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Goto);
};

// Wasm if-then-else construct.
// TODO(andreicostin): Add result type. The result type constrains possible
// combinations of then/else branches by requiring that they have the same
// type; i.e. consume the same number of arguments of the same types from the
// operand stack and likewise push back the same number or values of the
// same types.
class If : public Instruction {
 public:
  dart::SExpression* Serialize() override;

  InstructionList* test() { return test_; }
  InstructionList* then() { return test_; }
  InstructionList* otherwise() { return test_; }

 private:
  explicit If(BasicBlock* basic_block);

  InstructionList* const test_;
  InstructionList* const then_;
  InstructionList* const otherwise_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(If);
};

// Wasm function locals: local and param declarations used in function
// definitions. The two concepts have not been separated because in Wasm
// parameters and locals share the same indexing space of their containing
// function.
class Local : public dart::ZoneAllocated {
 public:
  enum class LocalKind { kLocal, kParam };

  dart::SExpression* Serialize();

  const char* name() const { return name_; }
  int index() const { return index_; }
  LocalKind kind() const { return kind_; }
  Function* function() const { return function_; }
  WasmModuleBuilder* module_builder() const;

 private:
  Local(Function* function,
        LocalKind kind,
        Type* type,
        const char* name,
        int index)
      : function_(function),
        name_(name),
        index_(index),
        kind_(kind),
        type_(type) {}

  Function* const function_;
  const char* name_;
  const uint32_t index_;
  const LocalKind kind_;
  Type* const type_;

  friend class Function;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Local);
};

class InstructionList : public dart::ZoneAllocated {
 public:
  dart::SExpression* Serialize();

  BasicBlock* block() const { return block_; }
  WasmModuleBuilder* module_builder() const;

  // One method for creating each type of Wasm instruction.
  // This abstracts away the need to pass parent pointers in
  // the constructors, as well as the need to allocate the
  // instructions in the correct zone.
  Instruction* AddLocalGet(Local* local);
  Instruction* AddLocalSet(Local* local);
  Instruction* AddInt32Add();
  Instruction* AddConstant(uint32_t value);
  Instruction* AddGoto(BasicBlock* target_block);
  Instruction* AddIf();

 private:
  explicit InstructionList(BasicBlock* block);

  BasicBlock* const block_;
  dart::GrowableArray<Instruction*> instructions_;

  friend class BasicBlock;  // For private constructor.
  friend class If;          // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(InstructionList);
};

// Wasm analogue of a CFG basic block. Initially, the correspondence between
// the two types of blocks is 1:1. Wasm Goto instructions can only occur as
// the last instruction in a block, with the exception of when the last
// instruction is an if statement with two branches, each being a Goto
// instruction (i.e. if ? then goto A else goto B). This is used to model a
// block ending in a conditional jump.
class BasicBlock : public dart::ZoneAllocated {
 public:
  dart::SExpression* Serialize();
  intptr_t block_id() const { return block_id_; }

  Function* function() const { return function_; }
  WasmModuleBuilder* module_builder() const;
  InstructionList* instructions() const { return instructions_; }

 private:
  BasicBlock(Function* function, intptr_t block_id);

  Function* const function_;
  const intptr_t block_id_;
  InstructionList* const instructions_;

  friend class Function;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(BasicBlock);
};

// Wasm function. Its body is represented by a list of Wasm BasicBlocks.
// In order for its serialization method to produce Wasm-compliant code,
// Linearize() has to be called prior to serialization.
class Function : public dart::ZoneAllocated {
 public:
  dart::SExpression* Serialize();

  Local* AddLocal(Local::LocalKind kind, Type* type, const char* name);
  BasicBlock* AddBlock(intptr_t block_id);

  Local* GetLocalByName(const char* name);
  Local* GetLocalByIndex(int id);
  BasicBlock* GetBlockById(intptr_t block_id);

  // Linearizes the block structure, replacing the blocks by a single block
  // without Wasm Goto instructions.
  void Linearize();

  WasmModuleBuilder* module_builder() const { return module_builder_; }

 private:
  Function(WasmModuleBuilder* module_builder,
           const char* name,
           uint32_t index,
           Type* result_type);

  WasmModuleBuilder* const module_builder_;
  const char* name_;
  const uint32_t index_;
  Type* const result_type_;
  dart::GrowableArray<Local*> locals_;
  dart::GrowableArray<BasicBlock*> body_;

  friend class WasmModuleBuilder;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Function);
};

// Provides higher level methods for building a Wasm module and
// serializing it. Instantiated as a field of the Precompiler.
class WasmModuleBuilder : public dart::ValueObject {
 public:
  explicit WasmModuleBuilder(dart::Thread* thread);
  dart::SExpression* Serialize();

  Function* AddFunction(const char* name, Type* result_type);

  PrimitiveType* i32() { return &i32_; }
  PrimitiveType* i64() { return &i64_; }
  PrimitiveType* f32() { return &f32_; }
  PrimitiveType* f64() { return &f64_; }

  dart::Thread* thread() const { return thread_; }
  dart::Zone* zone() const { return zone_; }

 private:
  PrimitiveType i32_;
  PrimitiveType i64_;
  PrimitiveType f32_;
  PrimitiveType f64_;

  dart::Thread* const thread_;
  dart::Zone* const zone_;
  dart::GrowableArray<Function*> functions_;
  dart::GrowableArray<Type*> types_;

  DISALLOW_COPY_AND_ASSIGN(WasmModuleBuilder);
};

}  // namespace wasm

#endif  // RUNTIME_VM_COMPILER_ASSEMBLER_ASSEMBLER_WASM_H_
