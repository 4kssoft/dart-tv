// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef RUNTIME_VM_COMPILER_ASSEMBLER_ASSEMBLER_WASM_H_
#define RUNTIME_VM_COMPILER_ASSEMBLER_ASSEMBLER_WASM_H_

#include "vm/compiler/backend/sexpression.h"
#include "vm/datastream.h"
#include "vm/thread.h"

namespace wasm {

// Forward declarations.
#define FOR_ALL_WASM_CONSTRUCTS(M)                                             \
  M(Type)                                                                      \
  M(ValueType)                                                                 \
  M(NumType)                                                                   \
  M(RefType)                                                                   \
  M(HeapType)                                                                  \
  M(Rtt)                                                                       \
  M(Field)                                                                     \
  M(FieldType)                                                                 \
  M(DefType)                                                                   \
  M(FuncType)                                                                  \
  M(StructType)                                                                \
  M(ArrayType)                                                                 \
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

// Note: all abstract types syntax is adapted from https://bit.ly/3cWcm6Q
// and https://github.com/WebAssembly/gc/blob/master/proposals/gc/MVP.md.

// Abstract base class for Wasm types.
class Type : public dart::ZoneAllocated {
 public:
  virtual dart::SExpression* Serialize() = 0;
  virtual void OutputBinary() = 0;
  virtual ~Type() = default;
  WasmModuleBuilder* module_builder() { return module_builder_; }

 protected:
  explicit Type(WasmModuleBuilder* module_builder)
      : module_builder_(module_builder) {}

 private:
  WasmModuleBuilder* const module_builder_;

  DISALLOW_COPY_AND_ASSIGN(Type);
};

// Abstract base class for Wasm types representing a value.
// - Abstract syntax:
//    value_type ::= <num_type> | <ref_type> | rtt <depth> <heap_type>
class ValueType : public Type {
 public:
  virtual dart::SExpression* Serialize() = 0;
  virtual void OutputBinary() = 0;
  virtual ~ValueType() = default;

 protected:
  explicit ValueType(WasmModuleBuilder* module_builder)
      : Type(module_builder) {}

 private:
  DISALLOW_COPY_AND_ASSIGN(ValueType);
};

// Class for Wasm numeric types: integers and floting points.
// - Abstract syntax:
//    num_type ::= i32 | i64 | f32 | f64 | v128
// TODO(andreicostin): Add v128 support when (and if) needed.
class NumType : public ValueType {
 public:
  dart::SExpression* Serialize() override;
  void OutputBinary() override;

 private:
  enum class Kind { kI32, kI64, kF32, kF64 };

  NumType(WasmModuleBuilder* module_builder, Kind kind)
      : ValueType(module_builder), kind_(kind) {}

  const Kind kind_;

  friend class WasmModuleBuilder;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(NumType);
};

// Class for Wasm reference types.
// - Abstract syntax:
//    ref_type ::= (ref null? <heap_type>)
// - Representation details:
//    heap_type_ should not be nullptr.
// Observe that one can not define a reference to an unboxed, say, i32.
class RefType : public ValueType {
 public:
  dart::SExpression* Serialize() override;
  void OutputBinary() override;

 private:
  RefType(WasmModuleBuilder* module_builder, bool nullable, HeapType* heap_type)
      : ValueType(module_builder),
        nullable_(nullable),
        heap_type_(ASSERT_NOTNULL(heap_type)) {}

  const bool nullable_;
  HeapType* const heap_type_;

  friend class WasmModuleBuilder;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(RefType);
};

// Class for Wasm heap types.
// - Syntax:
//    heap_type ::= func | extern | <typeidx> | any | eq | i31
// - Representation details:
//    The six options are distinguished by the variable kind_.
//    Either kind_ is kTypeidx, in which case def_type_ should point
//     to an appropriate defined type, or kind_ is not kTypeidx, in
//     which case def_type_ should be nullptr.
// Observe how typeids are not stored explicitly, but rather through
//  the pointer def_type_ to the corresponding defined type.
class HeapType : public Type {
 public:
  dart::SExpression* Serialize() override;
  void OutputBinary() override;

 private:
  enum class Kind { kFunc, kExtern, kTypeidx, kAny, kEq, kI31 };

  // Constructor for <typeidx> case of heap_type.
  HeapType(WasmModuleBuilder* module_builder, DefType* def_type)
      : Type(module_builder),
        kind_(Kind::kTypeidx),
        def_type_(ASSERT_NOTNULL(def_type)) {}

  // Constructor for the other cases of heap_type.
  HeapType(WasmModuleBuilder* module_builder, Kind kind)
      : Type(module_builder), kind_(kind), def_type_(nullptr) {
    ASSERT(kind != Kind::kTypeidx);
  }

  const Kind kind_;
  DefType* const def_type_;

  friend class WasmModuleBuilder;  // For private constructor.
  friend class RefType;            // For access to Kind and kind_.
  DISALLOW_COPY_AND_ASSIGN(HeapType);
};

// Class for Wasm runtime types.
class Rtt : public ValueType {
 public:
  dart::SExpression* Serialize() override;
  void OutputBinary() override;

 private:
  Rtt(WasmModuleBuilder* module_builder, intptr_t depth, HeapType* heap_type)
      : ValueType(module_builder), depth_(depth), heap_type_(heap_type) {}

  const intptr_t depth_;
  HeapType* const heap_type_;

  DISALLOW_COPY_AND_ASSIGN(Rtt);
};

// Class for a Wasm field of a struct type.
// It consists of a FieldType and an index of the field in the enclosing struct.
class Field : dart::ZoneAllocated {
 public:
  dart::SExpression* Serialize();
  void OutputBinary();

  StructType* struct_type() const { return struct_type_; }
  WasmModuleBuilder* module_builder() const;

 private:
  Field(StructType* struct_type, FieldType* field_type, uint32_t index)
      : struct_type_(struct_type), field_type_(field_type), index_(index) {}

  StructType* const struct_type_;
  FieldType* const field_type_;
  const uint32_t index_;

  friend class StructType;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Field);
};

// Class for Wasm field types. Fields are used inside both struct and
// array types. Esssentially, a combined <field_type> + <storage_type> +
// <packed_type> from the spec.
// - Abstract syntax:
//  field_type ::= <storage_type> | (mut <storage_type>)
//  storage_type ::= <value_type> | <packed_type>
//  packed_type ::= i8 | i16
// - Representation details:
//  mut_ represents whether the field is mutable or not.
//  value_type_ and packed_type_ collectively represent the
//   storage_type, as follows: either packed_type_ is kNoType
//   and value_type_ is not nullptr, or packed_type_ is not
//   kNoType and value_type_ is nullptr.
class FieldType : public Type {
 public:
  dart::SExpression* Serialize() override;
  void OutputBinary() override;

 private:
  enum class PackedType { kNoType, kI8, kI16 };

  // Constructor for <value_type> case of storage_type.
  FieldType(WasmModuleBuilder* module_builder, ValueType* value_type, bool mut)
      : Type(module_builder),
        value_type_(ASSERT_NOTNULL(value_type)),
        packed_type_(PackedType::kNoType),
        mut_(mut) {}

  // Constructor for <packed_type> case of storage_type.
  FieldType(WasmModuleBuilder* module_builder, PackedType packed_type, bool mut)
      : Type(module_builder),
        value_type_(nullptr),
        packed_type_(packed_type),
        mut_(mut) {
    ASSERT(packed_type_ != PackedType::kNoType);
  }

  ValueType* const value_type_;
  const PackedType packed_type_;
  const bool mut_;

  friend class WasmModuleBuilder;  // For private constructor.
  friend class StructType;         // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(FieldType);
};

// Class for Wasm defined types, which appear in the types section.
// - Abstract syntax:
//  def_type = <func_type> | <struct_type> | <array_type>
// - Representation details:
//  Each defined type stores in index_ its index in the types section.
class DefType : public Type {
 public:
  virtual dart::SExpression* Serialize() = 0;
  virtual void OutputBinary() = 0;
  virtual ~DefType() = default;

  uint32_t index() const { return index_; }

 protected:
  explicit DefType(WasmModuleBuilder* module_builder, uint32_t index)
      : Type(module_builder), index_(index) {}

 private:
  // All defined types have an index in the module.
  const uint32_t index_;

  DISALLOW_COPY_AND_ASSIGN(DefType);
};

// Class for Wasm function types.
// Notably, parameters are not named.
class FuncType : public DefType {
 public:
  dart::SExpression* Serialize() override;
  void OutputBinary() override;

  void AddParam(ValueType* param_type);

 private:
  FuncType(WasmModuleBuilder* module_builder,
           int index,
           ValueType* result_type);

  dart::GrowableArray<ValueType*> param_types_;
  ValueType* const result_type_;

  friend class WasmModuleBuilder;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(FuncType);
};

// Class for Wasm struct types.
class StructType : public DefType {
 public:
  dart::SExpression* Serialize() override;
  void OutputBinary() override;

  Field* AddField(FieldType* field_type);
  Field* AddField(ValueType* value_type, bool mut);
  Field* AddField(FieldType::PackedType packed_type, bool mut);

 private:
  StructType(WasmModuleBuilder* module_builder, int index);

  dart::GrowableArray<Field*> fields_;

  friend class WasmModuleBuilder;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(StructType);
};

// Class for Wasm array types.
class ArrayType : public DefType {
 public:
  dart::SExpression* Serialize() override;
  void OutputBinary() override;

 private:
  ArrayType(WasmModuleBuilder* module_builder, int index, FieldType* field_type)
      : DefType(module_builder, index), field_type_(field_type) {}

  FieldType* const field_type_;

  friend class WasmModuleBuilder;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(ArrayType);
};

// Abstract base class for Wasm instructions.
class Instruction : public dart::ZoneAllocated {
 public:
  virtual dart::SExpression* Serialize() = 0;
  virtual void OutputBinary() = 0;
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
  void OutputBinary() override;

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
  void OutputBinary() override;

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
  void OutputBinary() override;

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
  void OutputBinary() override;

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
  void OutputBinary() override;

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
  void OutputBinary() override;

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
  enum class Kind { kLocal, kParam };

  dart::SExpression* Serialize();
  void OutputBinary();

  const char* name() const { return name_; }
  int index() const { return index_; }
  Kind kind() const { return kind_; }
  Function* function() const { return function_; }
  WasmModuleBuilder* module_builder() const;

 private:
  Local(Function* function,
        Kind kind,
        ValueType* type,
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
  const Kind kind_;
  ValueType* const type_;

  friend class Function;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Local);
};

class InstructionList : public dart::ZoneAllocated {
 public:
  dart::SExpression* Serialize();
  void OutputBinary();

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
  void OutputBinary();

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

// Wasm function. Its body is represented as a list of Wasm BasicBlocks.
// In order for its serialization method to produce no unstructured
// instructions (i.e. jumps), Linearize() has to be called prior to
// serialization.
class Function : public dart::ZoneAllocated {
 public:
  dart::SExpression* Serialize();
  void OutputBinary();

  Local* AddLocal(Local::Kind kind, ValueType* type, const char* name);
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
           FuncType* type);

  WasmModuleBuilder* const module_builder_;
  const char* name_;
  const uint32_t index_;
  FuncType* const type_;
  dart::GrowableArray<Local*> locals_;
  dart::GrowableArray<BasicBlock*> body_;

  friend class WasmModuleBuilder;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Function);
};

// Provides higher level methods for building a Wasm module and
// serializing it. Instantiated as a field of the Precompiler.
class WasmModuleBuilder : public dart::ZoneAllocated {
 public:
  WasmModuleBuilder(dart::Zone* zone, uint8_t** buffer, dart::ReAlloc realloc);

  // After we add conversion straight to binary format, this will no longer
  // serve as the main way of outputting Wasm. Instead, it will be used as a
  // means of debugging. Consequently, it's not entirely compliant with actual
  // text format. Applies to all other Serialize() methods.
  dart::SExpression* Serialize();
  void OutputBinary();

  // Used to specify a field type, for use in array/struct field construction.
  FieldType* MakeFieldType(ValueType* value_type, bool mut);
  FieldType* MakeFieldType(FieldType::PackedType packed_type, bool mut);

  // Wrap a defined type into a heap type.
  HeapType* MakeHeapType(DefType* def_type);
  // Wrap a heap type into a reference type.
  RefType* MakeRefType(bool nullable, HeapType* heap_type);

  // Create defined types (push to the types_ array as a side effect).
  FuncType* MakeFuncType(ValueType* result_type);
  StructType* MakeStructType();
  ArrayType* MakeArrayType(FieldType* field_type);
  ArrayType* MakeArrayType(ValueType* value_type, bool mut);
  ArrayType* MakeArrayType(FieldType::PackedType packed_type, bool mut);

  Function* AddFunction(const char* name, FuncType* type);

  // Builtin types.
  NumType* i32() { return &i32_; }
  NumType* i64() { return &i64_; }
  NumType* f32() { return &f32_; }
  NumType* f64() { return &f64_; }
  RefType* funcref() { return &funcref_; }
  RefType* externref() { return &externref_; }
  RefType* anyref() { return &anyref_; }
  RefType* eqref() { return &eqref_; }
  RefType* i31ref() { return &i31ref_; }
  HeapType* func() { return &func_; }
  HeapType* ext() { return &ext_; }
  HeapType* any() { return &any_; }
  HeapType* eq() { return &eq_; }
  HeapType* i31() { return &i31_; }

  dart::Zone* zone() const { return zone_; }
  intptr_t bytes_written() const {
    return binary_output_stream_.bytes_written();
  }

 private:
  NumType i32_;
  NumType i64_;
  NumType f32_;
  NumType f64_;
  HeapType func_;
  HeapType ext_;
  HeapType any_;
  HeapType eq_;
  HeapType i31_;
  RefType funcref_;    // Type alias: funcref = (ref null func).
  RefType externref_;  // Type alias: externref = (ref null extern).
  RefType anyref_;     // Type alias: anyref = (ref null any).
  RefType eqref_;      // Type alias: eqref = (ref null eq).
  RefType i31ref_;     // Type alias: i31ref = (ref i31).

  dart::Zone* const zone_;
  dart::WriteStream binary_output_stream_;

  dart::GrowableArray<DefType*> types_;
  dart::GrowableArray<Function*> functions_;

  DISALLOW_COPY_AND_ASSIGN(WasmModuleBuilder);
};

}  // namespace wasm

#endif  // RUNTIME_VM_COMPILER_ASSEMBLER_ASSEMBLER_WASM_H_
