// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef RUNTIME_VM_COMPILER_ASSEMBLER_ASSEMBLER_WASM_H_
#define RUNTIME_VM_COMPILER_ASSEMBLER_ASSEMBLER_WASM_H_

#include "vm/compiler/backend/sexpression.h"
#include "vm/datastream.h"

namespace wasm {

void WasmTrace(const char* format, ...) PRINTF_ATTRIBUTE(1, 2);

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
  M(GlobalGet)                                                                 \
  M(IntOp)                                                                     \
  M(IntConstant)                                                               \
  M(Int32WrapInt64)                                                            \
  M(StructuredInstr)                                                           \
  M(Block)                                                                     \
  M(Loop)                                                                      \
  M(If)                                                                        \
  M(Br)                                                                        \
  M(RttCanon)                                                                  \
  M(RttSub)                                                                    \
  M(StructGet)                                                                 \
  M(StructSet)                                                                 \
  M(StructNewWithRtt)                                                          \
  M(Call)                                                                      \
  M(CallIndirect)                                                              \
  M(Drop)                                                                      \
  M(Return)                                                                    \
  M(RefNull)                                                                   \
  M(RefEq)                                                                     \
  M(RefCast)                                                                   \
  M(Table)                                                                     \
  M(SingleElemSegment)                                                         \
  M(Local)                                                                     \
  M(Global)                                                                    \
  M(InstructionList)                                                           \
  M(Function)                                                                  \
  M(WasmModuleBuilder)

#define FORWARD_DECLARATION(type) class type;
FOR_ALL_WASM_CONSTRUCTS(FORWARD_DECLARATION)
#undef FORWARD_DECLARATION

// Note: all abstract types syntax is adapted from https://bit.ly/3cWcm6Q
// and https://github.com/WebAssembly/gc/blob/master/proposals/gc/MVP.md.

// Abstract base class for Wasm types representing a value.
// - Abstract syntax:
//    value_type ::= <num_type> | <ref_type> | rtt <depth> <heap_type>
class ValueType : public dart::ZoneAllocated {
 public:
  virtual dart::SExpression* Serialize(dart::Zone* zone) = 0;
  virtual void OutputBinary(dart::WriteStream* stream) = 0;
  virtual ~ValueType() = default;

  virtual RefType* AsRefType() { return nullptr; }

 protected:
  ValueType() {}

 private:
  DISALLOW_COPY_AND_ASSIGN(ValueType);
};

// Class for Wasm numeric types: integers and floting points.
// - Abstract syntax:
//    num_type ::= i32 | i64 | f32 | f64 | v128
class NumType : public ValueType {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  enum class Kind { kI32, kI64, kF32, kF64, kV128 };

  NumType(Kind kind) : kind_(kind) {}

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
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

  RefType* AsRefType() override { return this; }
  HeapType* heap_type() const { return heap_type_; }

 private:
  RefType(bool nullable, HeapType* heap_type)
      : nullable_(nullable), heap_type_(ASSERT_NOTNULL(heap_type)) {}

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
class HeapType : public dart::ZoneAllocated {
 public:
  dart::SExpression* Serialize(dart::Zone* zone);
  void OutputBinary(dart::WriteStream* stream);

 private:
  // Note: V8 doesn't support any/anyref yet.
  // Reference: https://github.com/v8/v8/blob/master/src/wasm/wasm-constants.h
  enum class Kind { kFunc, kExtern, kTypeidx, kAny, kEq, kI31 };

  // Constructor for <typeidx> case of heap_type.
  HeapType(DefType* def_type)
      : kind_(Kind::kTypeidx), def_type_(ASSERT_NOTNULL(def_type)) {}

  // Constructor for the other cases of heap_type.
  HeapType(Kind kind) : kind_(kind), def_type_(nullptr) {
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
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

  intptr_t depth() const { return depth_; }
  HeapType* heap_type() const { return heap_type_; }

 private:
  Rtt(intptr_t depth, HeapType* heap_type)
      : depth_(depth), heap_type_(heap_type) {}

  const intptr_t depth_;
  HeapType* const heap_type_;

  friend class WasmModuleBuilder;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Rtt);
};

// Class for a Wasm field of a struct type.
// It consists of a FieldType and an index of the field in the enclosing struct.
class Field : public dart::ZoneAllocated {
 public:
  dart::SExpression* Serialize(dart::Zone* zone);
  void OutputBinary(dart::WriteStream* stream);

  Field(const Field& field)
      : struct_type_(field.struct_type_),
        field_type_(field.field_type_),
        index_(field.index_) {}

  StructType* struct_type() const { return struct_type_; }
  void set_struct_type_(StructType* struct_type) { struct_type_ = struct_type; }

  uint32_t index() const { return index_; }

 private:
  Field(StructType* struct_type, FieldType* field_type, uint32_t index)
      : struct_type_(struct_type), field_type_(field_type), index_(index) {}

  StructType* struct_type_;
  FieldType* const field_type_;
  const uint32_t index_;

  friend class StructType;  // For private constructor.
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
class FieldType : public dart::ZoneAllocated {
 public:
  dart::SExpression* Serialize(dart::Zone* zone);
  void OutputBinary(dart::WriteStream* stream);

 private:
  enum class PackedType { kNoType, kI8, kI16 };

  // Constructor for <value_type> case of storage_type.
  FieldType(ValueType* value_type, bool mut)
      : value_type_(ASSERT_NOTNULL(value_type)),
        packed_type_(PackedType::kNoType),
        mut_(mut) {}

  // Constructor for <packed_type> case of storage_type.
  FieldType(PackedType packed_type, bool mut)
      : value_type_(nullptr), packed_type_(packed_type), mut_(mut) {
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
class DefType : public dart::ZoneAllocated {
 public:
  virtual dart::SExpression* Serialize(dart::Zone* zone) = 0;
  virtual void OutputBinary(dart::WriteStream* stream) = 0;
  virtual ~DefType() = default;

  uint32_t index() const { return index_; }

 protected:
  DefType(dart::Zone* zone, uint32_t index) : zone_(zone), index_(index) {}

  dart::Zone* const zone_;

 private:
  // All defined types have an index in the module.
  const uint32_t index_;

  DISALLOW_COPY_AND_ASSIGN(DefType);
};

// Class for Wasm function types.
// Notably, parameters are not named.
class FuncType : public DefType {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

  void AddParam(ValueType* param_type);
  void AddResult(ValueType* result_type);

  intptr_t NumParams() const { return param_types_.length(); }
  ValueType* ParamTypeAt(intptr_t index) {
    ASSERT(0 <= index && index < NumParams());
    return param_types_.At(index);
  }
  intptr_t NumResults() const { return result_types_.length(); }
  ValueType* ResultTypeAt(intptr_t index) {
    ASSERT(0 <= index && index < NumResults());
    return result_types_.At(index);
  }

 private:
  FuncType(dart::Zone* zone, int index);

  dart::GrowableArray<ValueType*> param_types_;
  dart::GrowableArray<ValueType*> result_types_;

  friend class WasmModuleBuilder;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(FuncType);
};

// Class for Wasm struct types.
class StructType : public DefType {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

  // Copy fields of struct to a given destination struct. The enclosing
  // struct of the copied fields is set to point to dest.
  void CopyFieldsTo(StructType* dest);

  Field* AddField(FieldType* field_type);
  Field* AddField(ValueType* value_type, bool mut);
  Field* AddField(FieldType::PackedType packed_type, bool mut);

  dart::GrowableArray<Field*>& fields() { return fields_; }

 private:
  StructType(dart::Zone* zone, int index);

  dart::GrowableArray<Field*> fields_;

  friend class WasmModuleBuilder;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(StructType);
};

// Class for Wasm array types.
class ArrayType : public DefType {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  ArrayType(dart::Zone* zone, int index, FieldType* field_type)
      : DefType(zone, index), field_type_(field_type) {}

  FieldType* const field_type_;

  friend class WasmModuleBuilder;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(ArrayType);
};

// Abstract base class for Wasm instructions.
class Instruction : public dart::ZoneAllocated {
 public:
  virtual dart::SExpression* Serialize(dart::Zone* zone) = 0;
  virtual void OutputBinary(dart::WriteStream* stream) = 0;
  virtual ~Instruction() = default;

 protected:
  Instruction() {}

 private:
  DISALLOW_COPY_AND_ASSIGN(Instruction);
};

// Wasm local.get instruction.
class LocalGet : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  LocalGet(Local* local) : local_(local) {}

  Local* const local_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(LocalGet);
};

// Wasm local.set instruction.
class LocalSet : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  LocalSet(Local* local) : local_(local) {}

  Local* const local_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(LocalSet);
};

// Wasm global.get instruction.
class GlobalGet : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  GlobalGet(Global* global) : global_(global) {}

  Global* const global_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(GlobalGet);
};

// Wasm i{32/64} signed operations.
class IntOp : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

  enum class IntegerKind { kI32, kI64 };
  enum class OpKind {
    kAdd,   // +
    kSub,   // -
    kMult,  // *
    kDiv,   // ~/
    kMod,   // %
    kAnd,   // &
    kOr,    // |
    kXor,   // ^
    kEq,    // ==
    kNeq,   // !=
    kLt,    // <
    kGt,    // >
    kLe,    // <=
    kGe     // >=
  };

  static OpKind NegateOpKind(OpKind op_kind);

 private:
  IntOp(IntegerKind int_kind, OpKind op_kind)
      : int_kind_(int_kind), op_kind_(op_kind) {}

  const IntegerKind int_kind_;
  const OpKind op_kind_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(IntOp);
};

// Wasm i{32/64}.const instruction.
class IntConstant : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  enum class Kind { kI32, kI64 };

  IntConstant(Kind kind, uint64_t value) : kind_(kind), value_(value) {}

  const Kind kind_;
  const uint64_t value_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(IntConstant);
};

// Wasm i32.wrap_i64 instruction.
class Int32WrapInt64 : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  Int32WrapInt64() {}

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Int32WrapInt64);
};

// Abstract base class for Wasm block, loop and if instructions.
class StructuredInstr : public Instruction {
 public:
  virtual dart::SExpression* Serialize(dart::Zone* zone) = 0;
  virtual void OutputBinary(dart::WriteStream* stream) = 0;

  virtual ~StructuredInstr() = default;

  virtual InstructionList* body() = 0;

 protected:
  explicit StructuredInstr(FuncType* const block_type)
      : block_type_(block_type) {}

  FuncType* const block_type_;

 private:
  DISALLOW_COPY_AND_ASSIGN(StructuredInstr);
};

// Wasm block construct.
class Block : public StructuredInstr {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

  InstructionList* body() override { return body_; }

 private:
  Block(FuncType* const block_type, dart::Zone* zone);

  InstructionList* const body_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Block);
};

// Wasm loop construct.
class Loop : public StructuredInstr {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

  InstructionList* body() override { return body_; }

 private:
  Loop(FuncType* const block_type, dart::Zone* zone);

  InstructionList* const body_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Loop);
};

// Wasm if-then-else construct.
// The block type constrains possible combinations of then/else branches by
// requiring that they have the same type; i.e. consume the same number of
// arguments of the same types from the operand stack and likewise push back
// the same number or values of the same types.
class If : public StructuredInstr {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

  InstructionList* then() { return then_; }
  InstructionList* otherwise() { return otherwise_; }

  InstructionList* body() override { UNREACHABLE(); }

 private:
  If(FuncType* const block_type, dart::Zone* zone);

  InstructionList* const then_;
  InstructionList* const otherwise_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(If);
};

// Wasm br/br_if instructions.
class Br : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  Br(bool is_if, const uint32_t label) : is_if_(is_if), label_(label) {}

  const bool is_if_;  // true for br_if, false otherwise.
  const uint32_t label_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Br);
};

// Wasm GC extension rtt.canon instruction.
class RttCanon : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  explicit RttCanon(HeapType* type) : type_(type) {}

  HeapType* const type_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(RttCanon);
};

// Wasm GC extension rtt.sub instruction.
class RttSub : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  RttSub(intptr_t depth, HeapType* supertype, HeapType* type)
      : depth_(depth), supertype_(supertype), type_(type) {}

  const intptr_t depth_;
  HeapType* const supertype_;
  HeapType* const type_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(RttSub);
};

// Wasm GC extension struct.get instruction.
class StructGet : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  StructGet(StructType* struct_type, Field* field)
      : struct_type_(struct_type), field_(field) {}

  StructType* const struct_type_;
  // This might be a field from a struct which struct_type inherits from.
  // Due to structural prefix subtyping in Wasm, their <fieldidx> agree,
  // meaning that they can be used interchangeably for output purposes.
  Field* const field_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(StructGet);
};

// Wasm GC extension struct.set instruction.
class StructSet : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  StructSet(StructType* struct_type, Field* field)
      : struct_type_(struct_type), field_(field) {}

  StructType* const struct_type_;
  // This might be a field from a struct which struct_type inherits from.
  // Due to structural prefix subtyping in Wasm, their <fieldidx> agree,
  // meaning that they can be used interchangeably for output purposes.
  Field* const field_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(StructSet);
};

// Wasm struct.new_with_rtt/new_default_with_rtt instructions.
class StructNewWithRtt : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  StructNewWithRtt(StructType* struct_type, bool def)
      : struct_type_(struct_type), def_(def) {}

  StructType* const struct_type_;
  const bool def_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(StructNewWithRtt);
};

// Wasm call instruction.
class Call : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  Call(Function* function) : function_(function) {}

  Function* const function_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Call);
};

// Wasm call_indirect instruction.
class CallIndirect : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  CallIndirect(FuncType* func_type) : func_type_(func_type) {}

  FuncType* const func_type_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(CallIndirect);
};

// Wasm drop instruction.
class Drop : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  Drop() {}

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Drop);
};

// Wasm return instruction.
class Return : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  Return() {}

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Return);
};

// Wasm GC extension ref.null instruction.
class RefNull : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  explicit RefNull(HeapType* type) : type_(type) {}

  HeapType* const type_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(RefNull);
};

// Wasm GC extension ref.eq instruction.
class RefEq : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  RefEq() {}

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(RefEq);
};

// Wasm GC extension ref.cast instruction.
class RefCast : public Instruction {
 public:
  dart::SExpression* Serialize(dart::Zone* zone) override;
  void OutputBinary(dart::WriteStream* stream) override;

 private:
  RefCast(HeapType* from_type, HeapType* to_type)
      : from_type_(from_type), to_type_(to_type) {}

  HeapType* const from_type_;
  HeapType* const to_type_;

  friend InstructionList;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(RefCast);
};

// Wasm Table.
class Table : public dart::ZoneAllocated {
  dart::SExpression* Serialize(dart::Zone* zone);
  void OutputBinary(dart::WriteStream* stream);

 private:
  Table(uint32_t min_size, uint32_t max_size)
      : min_size_(min_size), max_size_(max_size) {}

  const uint32_t min_size_;
  const uint32_t max_size_;  // In our implementation, we
                             // require an explicit maximum size.

  friend WasmModuleBuilder;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Table);
};

// Wasm single element table initialization segment.
class SingleElemSegment : public dart::ZoneAllocated {
  dart::SExpression* Serialize(dart::Zone* zone);
  void OutputBinary(dart::WriteStream* stream);

 private:
  SingleElemSegment(uint32_t offset, Function* function)
      : offset_(offset), function_(function) {}

  // In the current Wasm specification, there can only be one table.
  // Table* const table_;
  // The Wasm specification gives this as an initializer expression.
  const uint32_t offset_;
  // The Wasm specification allows initializing more than a single table
  // position per segment. Our use case doesn't require this additional
  // feature, and hence the prefix 'Single' in the name of this class.
  Function* const function_;

  friend WasmModuleBuilder;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(SingleElemSegment);
};

// Wasm function locals: local and param declarations used in function
// definitions. The two concepts have not been separated because in Wasm
// parameters and locals share the same indexing space of their containing
// function.
class Local : public dart::ZoneAllocated {
 public:
  enum class Kind { kLocal, kParam };

  dart::SExpression* Serialize(dart::Zone* zone);
  void OutputBinary(dart::WriteStream* stream);

  Function* function() const { return function_; }
  const char* name() const { return name_; }
  uint32_t index() const { return index_; }
  Kind kind() const { return kind_; }
  ValueType* type() const { return type_; }

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

// Class for Wasm globals, which can be either mutable or immutable,
// and are initialized by a constant initializer expression.
class Global : public dart::ZoneAllocated {
 public:
  dart::SExpression* Serialize(dart::Zone* zone);
  void OutputBinary(dart::WriteStream* stream);

  InstructionList* init() const { return init_; }
  uint32_t index() const { return index_; }

 private:
  Global(dart::Zone* zone, ValueType* type, bool mut, uint32_t index);

  ValueType* const type_;
  const bool mut_;
  InstructionList* const init_;
  uint32_t index_;

  friend class WasmModuleBuilder;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Global);
};

// Wasm instruction list.
class InstructionList : public dart::ZoneAllocated {
 public:
  dart::SExpression* Serialize(dart::Zone* zone);
  void OutputBinary(dart::WriteStream* stream);

  // One method for creating each type of Wasm instruction.
  // This abstracts away the need to allocate the
  // instructions in the correct zone.
  LocalGet* AddLocalGet(Local* local);
  LocalSet* AddLocalSet(Local* local);
  GlobalGet* AddGlobalGet(Global* global);
  IntOp* AddIntOp(IntOp::IntegerKind integer_kind, IntOp::OpKind op_kind);
  IntConstant* AddI32Constant(uint32_t value);
  IntConstant* AddI64Constant(uint64_t value);
  Int32WrapInt64* AddInt32WrapInt64();
  Block* AddBlock(FuncType* block_type);
  Loop* AddLoop(FuncType* block_type);
  If* AddIf(FuncType* block_type);
  RttCanon* AddRttCanon(HeapType* type);
  RttSub* AddRttSub(uint32_t depth, HeapType* supertype, HeapType* type);
  StructGet* AddStructGet(StructType* struct_type, Field* field);
  StructSet* AddStructSet(StructType* struct_type, Field* field);
  StructNewWithRtt* AddStructNewWithRtt(StructType* struct_type);
  StructNewWithRtt* AddStructNewDefaultWithRtt(StructType* struct_type);
  Call* AddCall(Function* function);
  CallIndirect* AddCallIndirect(FuncType* function_type);
  Drop* AddDrop();
  Return* AddReturn();
  Br* AddBr(uint32_t label);
  Br* AddBrIf(uint32_t label);
  RefNull* AddRefNull(HeapType* type);
  RefEq* AddRefEq();
  RefCast* AddRefCast(HeapType* from_type, HeapType* to_type);

  dart::GrowableArray<Instruction*>& instructions() { return instructions_; }

 private:
  explicit InstructionList(dart::Zone* zone);

  dart::Zone* const zone_;
  dart::GrowableArray<Instruction*> instructions_;

  friend class Function;  // For private constructor.
  friend class Block;     // For private constructor.
  friend class Loop;      // For private constructor.
  friend class If;        // For private constructor.
  friend class Global;    // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(InstructionList);
};

// Wasm function. Its body is represented as an InstructionList.
// When imported_module_name_ is not null the function is imported from JS.
// In that case, the function is also body-less and will not be part of binary
// or serialization output. Imported functions need to precede all other
// functions, as per the Wasm specification.
class Function : public dart::ZoneAllocated {
 public:
  dart::SExpression* Serialize(dart::Zone* zone);
  // Outputs the body of the function, as part of the Wasm Code section.
  // Note that this skips outputing type_, since that goes into the Function
  // section of binary Wasm.
  void OutputBinary(dart::WriteStream* stream);

  Local* AddLocal(Local::Kind kind, ValueType* type, const char* name);

  Local* GetLocalByName(const char* name);
  Local* GetLocalByIndex(int id);

  InstructionList* MakeNewBodyAndClearLocals();

  uint32_t index() const { return index_; }
  FuncType* type() const { return type_; }
  void set_type(FuncType* type) { type_ = type; }
  InstructionList* body() const { return body_; }
  bool IsImported() const { return imported_module_name_ != nullptr; }

 private:
  Function(dart::Zone* zone,
           const char* module_name,
           const char* name,
           uint32_t index,
           FuncType* type);

  dart::Zone* const zone_;
  const char* imported_module_name_;
  const char* name_;
  const uint32_t index_;
  FuncType* type_;
  dart::GrowableArray<Local*> locals_;
  InstructionList* body_;

  friend class WasmModuleBuilder;  // For private constructor.
  DISALLOW_COPY_AND_ASSIGN(Function);
};

// Provides higher level methods for building a Wasm module and
// serializing it. Instantiated as a field of the Precompiler.
class WasmModuleBuilder : public dart::ValueObject {
 public:
  WasmModuleBuilder(dart::Zone* zone);

  // After we add conversion straight to binary format, this will no longer
  // serve as the main way of outputting Wasm. Instead, it will be used as a
  // means of debugging. Consequently, it's not entirely compliant with actual
  // text format. Applies to all other Serialize() methods.
  dart::SExpression* Serialize(dart::Zone* zone);
  void OutputBinary(dart::WriteStream* stream);

  // Used to specify a field type, for use in array/struct field construction.
  FieldType* MakeFieldType(ValueType* value_type, bool mut);
  FieldType* MakeFieldType(FieldType::PackedType packed_type, bool mut);

  // Wrap a defined type into a heap type.
  HeapType* MakeHeapType(DefType* def_type);
  // Wrap a heap type into a reference type.
  RefType* MakeRefType(bool nullable, HeapType* heap_type);
  // Make a reference to a user-defined type.
  RefType* MakeRefType(bool nullable, DefType* def_type);

  // Convenience methods for defining the tree structure of rtts.
  Global* MakeRttCanon(StructType* type);
  Global* MakeRttChild(StructType* type, Global* parent_global);

  // Create defined types (push to the types_ array as a side effect).
  FuncType* MakeFuncType();
  StructType* MakeStructType();
  ArrayType* MakeArrayType(FieldType* field_type);
  ArrayType* MakeArrayType(ValueType* value_type, bool mut);
  ArrayType* MakeArrayType(FieldType::PackedType packed_type, bool mut);

  // Create global definition.
  Global* AddGlobal(ValueType* type, bool mut);

  // Create function with the given name and function type.
  Function* AddFunction(const char* name, FuncType* type);

  // Create an imported function with the given name, module name and
  // function type.
  Function* AddImportedFunction(const char* module_name,
                                const char* name,
                                FuncType* type);

  Function* start_function() const { return start_function_; }
  void set_start_function(Function* function) { start_function_ = function; }

  // Create table in the table section. Can be called at most once per module.
  Table* AddFunctionsTable(uint32_t min_size, uint32_t max_size);

  // Set table value at the given offset to point to the given function.
  // Requires the table to have been created beforehand.
  SingleElemSegment* AddElemTableInitializer(uint32_t offset,
                                             Function* function);

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

 private:
  // Type section consists of a list of user-defined type definitions.
  void OutputTypeSection(dart::WriteStream* stream);
  // Import section consists of a list of imported functions, tables, memories,
  // and globals.
  void OutputImportSection(dart::WriteStream* stream);
  // Function section consists of a list of the types of all user-defined
  // functions. Note that actual code for the functions resides in the
  // Code section.
  void OutputFunctionSection(dart::WriteStream* stream);
  // Table section declares the global function references table, if present.
  // Its initialization is done in the Element section.
  void OutputTableSection(dart::WriteStream* stream);
  // Global section consists of a list of global variable definitions.
  // Each global can be either mutable or immutable, and is initialized
  // by a constant initializer expression.
  void OutputGlobalSection(dart::WriteStream* stream);
  // Start section consists of the index of the start function, if any.
  void OutputStartSection(dart::WriteStream* stream);
  // Element section consists of initializers for the Table section.
  void OutputElementSection(dart::WriteStream* stream);
  // Code section consists of a list with one entry per user-defined function.
  // Each entry consists of a description of the function locals (consisting
  // of the type for each local) and the contents of the function body.
  void OutputCodeSection(dart::WriteStream* stream);

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

  Function* start_function_;
  intptr_t num_imported_functions_;
  intptr_t num_non_imported_functions_;

  dart::Zone* const zone_;

  dart::GrowableArray<DefType*> types_;
  dart::GrowableArray<Function*> functions_;
  // The Wasm Core specification restricts the number of tables to at most one.
  // (see https://webassembly.github.io/spec/core/syntax/types#table-types).
  Table* table_;
  dart::GrowableArray<Global*> globals_;
  dart::GrowableArray<SingleElemSegment*> elem_segments_;

  friend class PushBytecountFrontScope;  // For access to replacing the
                                         // current binary_output_stream_.
  DISALLOW_COPY_AND_ASSIGN(WasmModuleBuilder);
};

}  // namespace wasm

#endif  // RUNTIME_VM_COMPILER_ASSEMBLER_ASSEMBLER_WASM_H_
