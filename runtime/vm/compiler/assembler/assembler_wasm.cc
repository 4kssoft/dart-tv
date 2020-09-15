// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/compiler/assembler/assembler_wasm.h"

#include "vm/log.h"

// In a few cases module_builder() will not be safe to call just yet
// (i.e. in a constructor initializer list), so care has to be taken
// when using this macro.
#define Z (module_builder()->zone())

#if defined(_MSC_VER)
#define WASM_TRACE(format, ...)                                                \
  if (FLAG_trace_wasm_compilation) {                                           \
    THR_Print(format, __VA_ARGS__);                                            \
  }
#else
#define WASM_TRACE(format, ...)                                                \
  if (FLAG_trace_wasm_compilation) {                                           \
    THR_Print(format, ##__VA_ARGS__);                                          \
  }
#endif

#define WRITE_BYTE(m)                                                          \
  module_builder()->binary_output_stream()->WriteFixed(static_cast<uint8_t>(m))
#define WRITE_UNSIGNED(m)                                                      \
  module_builder()->binary_output_stream()->WriteUnsigned(m)

// It is not permitted to have two occurences of this macro in the same
// enclosing scope. This is enforced by a variable name clash if this is
// not respected. This requires users to explicitly open a new scope whenever
// they need multiply-nested PushBytecountFrontScopes, consistent with the
// example uses below.
#define WRITE_BYTECOUNT()                                                      \
  PushBytecountFrontScope push_bytecount_front_scope(module_builder())

namespace wasm {
namespace {
using ::dart::FLAG_trace_wasm_compilation;  // For use in the WASM_TRACE macro.
using ::dart::Log;                          // For use in the WASM_TRACE macro.
using ::dart::OS;
using ::dart::ReAlloc;
using ::dart::SExpInteger;  // Uses int64_t internally, so it should fit most
                            // serialized Wasm integers.
using ::dart::SExpList;
using ::dart::SExpression;
using ::dart::SExpSymbol;
using ::dart::Zone;
using ::dart::ZoneWriteStream;
}  // namespace

// PushBytecountFrontScope is an instance of the RAII programming pattern.
// It is used in the WRITE_BYTECOUNT() macro, which is to only be used
// inside the OutputWasm() methods of the Wasm classes.
// Example usage and semantics:
//
//  The following *scoped* code inside an OutputWasm() method:
//  {
//    WRITE_BYTECOUNT();
//    WRITE_UNSIGNED(23);
//    WRITE_UNSIGNED(98);
//  }
//
//  Has the same effect as the following:
//  {
//     WRITE_UNSIGNED(2);  // Since 2 bytes follow in the enclosing scope.
//     WRITE_UNSIGNED(23);
//     WRITE_UNSIGNED(98);
//  }
//
//  Both code fragments write the list [2, 23, 98].
class PushBytecountFrontScope : public dart::ValueObject {
 public:
  explicit PushBytecountFrontScope(WasmModuleBuilder* module_builder);
  ~PushBytecountFrontScope();

 private:
  WasmModuleBuilder* const module_builder_;
  uint8_t* replacement_buffer_;
  dart::ZoneWriteStream* const old_binary_output_stream;

  DISALLOW_COPY_AND_ASSIGN(PushBytecountFrontScope);
};

PushBytecountFrontScope::PushBytecountFrontScope(
    WasmModuleBuilder* module_builder)
    : module_builder_(ASSERT_NOTNULL(module_builder)),
      replacement_buffer_(nullptr),
      old_binary_output_stream(module_builder_->binary_output_stream_) {
  module_builder_->binary_output_stream_ = new (module_builder_->zone())
      ZoneWriteStream(&replacement_buffer_, module_builder_->realloc_, 16);
}

PushBytecountFrontScope::~PushBytecountFrontScope() {
  const intptr_t bytes_written =
      module_builder_->binary_output_stream_->bytes_written();
  WASM_TRACE("Copying %" Pd " bytes to parent WriteStream.\n", bytes_written);
  // The Wasm specification treats bytes_written as an unsigned 32 bit integer.
  // While an overflow is technically possible, code of this size is unlikely
  // to occur in practice.
  old_binary_output_stream->WriteUnsigned(bytes_written);
  old_binary_output_stream->WriteBytes(replacement_buffer_, bytes_written);
  module_builder_->binary_output_stream_ = old_binary_output_stream;
}

SExpression* NumType::Serialize() {
  switch (kind_) {
    case Kind::kI32:
      return new (Z) SExpSymbol("i32");
    case Kind::kI64:
      return new (Z) SExpSymbol("i64");
    case Kind::kF32:
      return new (Z) SExpSymbol("f32");
    case Kind::kF64:
      return new (Z) SExpSymbol("f64");
    default:
      UNREACHABLE();
  }
}

void NumType::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

SExpression* RefType::Serialize() {
  ASSERT(heap_type_ != nullptr);
  switch (heap_type_->kind_) {
    case HeapType::Kind::kFunc:
      return new (Z) SExpSymbol("funcref");
    case HeapType::Kind::kExtern:
      return new (Z) SExpSymbol("externref");
    case HeapType::Kind::kTypeidx: {
      const auto sexp = new (Z) SExpList(Z);
      sexp->Add(new (Z) SExpSymbol("ref"));
      if (nullable_) {
        sexp->Add(new (Z) SExpSymbol("null"));
      }
      sexp->Add(heap_type_->Serialize());
      return sexp;
    }
    case HeapType::Kind::kAny:
      return new (Z) SExpSymbol("anyref");
    case HeapType::Kind::kEq:
      return new (Z) SExpSymbol("eqref");
    case HeapType::Kind::kI31:
      return new (Z) SExpSymbol("i31ref");
    default:
      UNREACHABLE();
  }
}

void RefType::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

SExpression* HeapType::Serialize() {
  switch (kind_) {
    case Kind::kFunc:
      return new (Z) SExpSymbol("func");
    case Kind::kExtern:
      return new (Z) SExpSymbol("extern");
    case Kind::kTypeidx:
      return new (Z) SExpInteger(def_type_->index());
    case Kind::kAny:
      return new (Z) SExpSymbol("any");
    case Kind::kEq:
      return new (Z) SExpSymbol("eq");
    case Kind::kI31:
      return new (Z) SExpSymbol("i31");
    default:
      UNREACHABLE();
  }
}

void HeapType::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

SExpression* Rtt::Serialize() {
  const auto sexp = new (Z) SExpList(Z);
  sexp->Add(new SExpSymbol("rtt"));
  sexp->Add(new SExpInteger(depth_));
  sexp->Add(heap_type_->Serialize());
  return sexp;
}

void Rtt::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

SExpression* Field::Serialize() {
  return field_type_->Serialize();
}

void Field::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

WasmModuleBuilder* Field::module_builder() const {
  return struct_type_->module_builder();
}

SExpression* FieldType::Serialize() {
  SExpression* sexp = nullptr;
  switch (packed_type_) {
    case PackedType::kNoType:
      sexp = value_type_->Serialize();
      break;
    case PackedType::kI8:
      sexp = new (Z) SExpSymbol("i8");
      break;
    case PackedType::kI16:
      sexp = new (Z) SExpSymbol("i16");
      break;
    default:
      UNREACHABLE();
  }
  // Add mutable atom if present.
  if (mut_) {
    const auto sexp_list = new (Z) SExpList(Z);
    sexp_list->Add(new (Z) SExpSymbol("mut"));
    sexp_list->Add(sexp);
    return sexp_list;
  } else {
    return sexp;
  }
}

void FieldType::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

FuncType::FuncType(WasmModuleBuilder* module_builder,
                   int index,
                   ValueType* result_type)
    : DefType(module_builder, index),
      param_types_(module_builder->zone(), 16),
      result_type_(result_type) {}

SExpression* FuncType::Serialize() {
  const auto sexp = new (Z) SExpList(Z);
  sexp->Add(new (Z) SExpSymbol("func"));
  // Add "param" atoms.
  for (ValueType* param_type : param_types_) {
    const auto atom = new (Z) SExpList(Z);
    atom->Add(new (Z) SExpSymbol("param"));
    atom->Add(param_type->Serialize());
    sexp->Add(atom);
  }
  // Add "result" atom.
  const auto atom = new (Z) SExpList(Z);
  atom->Add(new (Z) SExpSymbol("result"));
  atom->Add(result_type_->Serialize());
  sexp->Add(atom);
  return sexp;
}

void FuncType::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

void FuncType::AddParam(ValueType* param_type) {
  param_types_.Add(param_type);
}

StructType::StructType(WasmModuleBuilder* module_builder, int index)
    : DefType(module_builder, index), fields_(module_builder->zone(), 16) {}

SExpression* StructType::Serialize() {
  const auto sexp = new (Z) SExpList(Z);
  sexp->Add(new (Z) SExpSymbol("struct"));
  for (Field* field : fields_) {
    sexp->Add(field->Serialize());
  }
  return sexp;
}

void StructType::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

SExpression* ArrayType::Serialize() {
  const auto sexp = new (Z) SExpList(Z);
  sexp->Add(new (Z) SExpSymbol("array"));
  sexp->Add(field_type_->Serialize());
  return sexp;
}

void ArrayType::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

Field* StructType::AddField(FieldType* field_type) {
  fields_.Add(new (Z) Field(this, field_type, fields_.length()));
  return fields_.Last();
}
Field* StructType::AddField(ValueType* value_type, bool mut) {
  return AddField(module_builder()->MakeFieldType(value_type, mut));
}
Field* StructType::AddField(FieldType::PackedType packed_type, bool mut) {
  return AddField(module_builder()->MakeFieldType(packed_type, mut));
}

WasmModuleBuilder* Instruction::module_builder() const {
  return block_->module_builder();
}

SExpression* LocalGet::Serialize() {
  return new (Z) SExpSymbol(OS::SCreate(Z, "local.get $%s", local_->name()));
}

void LocalGet::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

SExpression* LocalSet::Serialize() {
  return new (Z) SExpSymbol(OS::SCreate(Z, "local.set $%s", local_->name()));
}

void LocalSet::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

SExpression* Int32Add::Serialize() {
  return new (Z) SExpSymbol("i32.add");
}

void Int32Add::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

SExpression* Constant::Serialize() {
  return new (Z) SExpSymbol(OS::SCreate(Z, "i32.const %" Pu32, value_));
}

void Constant::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

// Serializing a Goto doesn't produce Wasm-compliant code, but should be
// helpful for testing.
SExpression* Goto::Serialize() {
  return new (Z)
      SExpSymbol(OS::SCreate(Z, "goto B%" Pd, target_block_->block_id()));
}

void Goto::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

If::If(BasicBlock* basic_block)
    : Instruction(basic_block),
      test_(new (Z) InstructionList(block())),
      then_(new (Z) InstructionList(block())),
      otherwise_(new (Z) InstructionList(block())) {}

// TODO(andreicostin): Add and serialize result type.
SExpression* If::Serialize() {
  // Serialize test condition.
  const auto sexp_test = new (Z) SExpList(Z);
  sexp_test->Add(test_->Serialize());

  // Serialize then branch.
  const auto sexp_then = new (Z) SExpList(Z);
  sexp_then->Add(new SExpSymbol("then"));
  sexp_then->Add(then_->Serialize());

  // Serialize otherwise branch.
  const auto sexp_otherwise = new (Z) SExpList(Z);
  sexp_then->Add(new SExpSymbol("else"));
  sexp_then->Add(otherwise_->Serialize());

  // Produce final SExpression.
  const auto sexp = new (Z) SExpList(Z);
  sexp->Add(new SExpSymbol("if"));
  sexp->Add(sexp_test);
  sexp->Add(sexp_then);
  sexp->Add(sexp_otherwise);
  return sexp;
}

void If::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

WasmModuleBuilder* Local::module_builder() const {
  return function_->module_builder();
}

SExpression* Local::Serialize() {
  const auto sexp = new (Z) SExpList(Z);
  switch (kind_) {
    case Kind::kLocal:
      sexp->Add(new (Z) SExpSymbol("local"));
      break;
    case Kind::kParam:
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

void Local::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

InstructionList::InstructionList(BasicBlock* block)
    : block_(block), instructions_(Z, 16) {}

WasmModuleBuilder* InstructionList::module_builder() const {
  return block_->module_builder();
}

SExpression* InstructionList::Serialize() {
  const auto sexp = new (Z) SExpList(Z);
  for (Instruction* instr : instructions_) {
    sexp->Add(instr->Serialize());
  }
  return sexp;
}

void InstructionList::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
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
  const auto sexp = new (Z) SExpList(Z);
  sexp->Add(new (Z) SExpSymbol(OS::SCreate(Z, "B%" Pd, block_id_)));
  sexp->Add(instructions_->Serialize());
  return sexp;
}

void BasicBlock::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

Function::Function(WasmModuleBuilder* module_builder,
                   const char* name,
                   uint32_t index,
                   FuncType* type)
    : module_builder_(module_builder),
      name_(name),
      index_(index),
      type_(type),
      locals_(module_builder->zone(), 16),
      body_(module_builder->zone(), 16) {}

SExpression* Function::Serialize() {
  const auto sexp = new (Z) SExpList(Z);
  sexp->Add(new (Z) SExpSymbol("func"));
  if (strcmp(name_, "") != 0) {
    sexp->Add(new (Z) SExpSymbol(OS::SCreate(Z, "$%s", name_)));
  }
  // Serialize type.
  const auto sexp_type = new (Z) SExpList(Z);
  sexp_type->Add(new (Z) SExpSymbol("type"));
  sexp_type->Add(new (Z) SExpInteger(type_->index()));
  sexp->Add(sexp_type);
  // Serialize locals.
  for (Local* it : locals_) {
    if (it->kind() == Local::Kind::kLocal) {
      sexp->Add(it->Serialize());
    }
  }
  // Serialize the body.
  // TODO(andreicostin): This should be changed in the case when there
  // is only one basic block to not output the "blocks" statement.
  // This will always be the case after Function::Linearize() has been
  // called on the function.
  const auto sexp_body = new (Z) SExpList(Z);
  sexp_body->Add(new (Z) SExpSymbol("blocks"));
  for (BasicBlock* it : body_) {
    sexp_body->Add(it->Serialize());
  }
  sexp->Add(sexp_body);
  return sexp;
}

void Function::OutputBinary() {
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

Local* Function::AddLocal(Local::Kind kind, ValueType* type, const char* name) {
  // No further params can be declared after the first
  // local in a Wasm function header.
  ASSERT(kind == Local::Kind::kLocal || locals_.is_empty() ||
         locals_.Last()->kind() == Local::Kind::kParam);
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

WasmModuleBuilder::WasmModuleBuilder(Zone* zone,
                                     uint8_t** binary_output_buffer,
                                     ReAlloc realloc)
    : i32_(this, NumType::Kind::kI32),
      i64_(this, NumType::Kind::kI64),
      f32_(this, NumType::Kind::kF32),
      f64_(this, NumType::Kind::kF64),
      func_(this, HeapType::Kind::kFunc),
      ext_(this, HeapType::Kind::kExtern),
      any_(this, HeapType::Kind::kAny),
      eq_(this, HeapType::Kind::kEq),
      i31_(this, HeapType::Kind::kI31),
      funcref_(this, /*nullable =*/true, &func_),
      externref_(this, /*nullable =*/true, &ext_),
      anyref_(this, /*nullable =*/true, &any_),
      eqref_(this, /*nullable =*/true, &eq_),
      i31ref_(this, /*nullable =*/false, &i31_),
      zone_(zone),
      binary_output_stream_(
          binary_output_buffer == nullptr
              ? nullptr
              : new (Z) ZoneWriteStream(binary_output_buffer, realloc, 16)),
      realloc_(realloc),
      types_(zone, 16),
      functions_(zone, 16) {}

SExpression* WasmModuleBuilder::Serialize() {
  const auto sexp = new (Z) SExpList(Z);
  sexp->Add(new (Z) SExpSymbol("module"));
  // Types section.
  for (DefType* def_type : types_) {
    const auto sexp_type = new (Z) SExpList(Z);
    sexp_type->Add(new (Z) SExpSymbol("type"));
    sexp_type->Add(def_type->Serialize());
    sexp->Add(sexp_type);
  }
  // Functions section.
  // Note that, in binary format, the bodies
  // of the functions are stored separately, in the code section.
  for (Function* fct : functions_) {
    sexp->Add(fct->Serialize());
  }
  return sexp;
}

void WasmModuleBuilder::OutputBinary() {
  ASSERT(binary_output_stream_ != nullptr);
  // Testing code for the new RAII class - correctly pushes 8 front.
  WRITE_BYTECOUNT();
  // Magic.
  WRITE_BYTE('\0');
  WRITE_BYTE('a');
  WRITE_BYTE('s');
  WRITE_BYTE('m');
  // Version.
  WRITE_BYTE(0x01);
  WRITE_BYTE(0x00);
  WRITE_BYTE(0x00);
  WRITE_BYTE(0x00);
  // TODO(andreicostin): Write the logic.
  UNIMPLEMENTED();
}

FieldType* WasmModuleBuilder::MakeFieldType(ValueType* value_type, bool mut) {
  return new (Z) FieldType(this, value_type, mut);
}

FieldType* WasmModuleBuilder::MakeFieldType(FieldType::PackedType packed_type,
                                            bool mut) {
  return new (Z) FieldType(this, packed_type, mut);
}

ArrayType* WasmModuleBuilder::MakeArrayType(FieldType* field_type) {
  const auto array_type = new (Z) ArrayType(this, types_.length(), field_type);
  types_.Add(array_type);
  return array_type;
}

ArrayType* WasmModuleBuilder::MakeArrayType(ValueType* value_type, bool mut) {
  return MakeArrayType(MakeFieldType(value_type, mut));
}
ArrayType* WasmModuleBuilder::MakeArrayType(FieldType::PackedType packed_type,
                                            bool mut) {
  return MakeArrayType(MakeFieldType(packed_type, mut));
}

HeapType* WasmModuleBuilder::MakeHeapType(DefType* def_type) {
  return new (Z) HeapType(this, def_type);
}

RefType* WasmModuleBuilder::MakeRefType(bool nullable, HeapType* heap_type) {
  return new (Z) RefType(this, nullable, heap_type);
}

FuncType* WasmModuleBuilder::MakeFuncType(ValueType* result_type) {
  const auto fct_type = new (Z) FuncType(this, types_.length(), result_type);
  types_.Add(fct_type);
  return fct_type;
}

StructType* WasmModuleBuilder::MakeStructType() {
  const auto str_type = new (Z) StructType(this, types_.length());
  types_.Add(str_type);
  return str_type;
}

Function* WasmModuleBuilder::AddFunction(const char* name, FuncType* type) {
  functions_.Add(new (Z) Function(this, name, functions_.length(), type));
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
