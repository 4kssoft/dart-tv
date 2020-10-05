// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/compiler/assembler/assembler_wasm.h"
#include "platform/text_buffer.h"

#include "vm/log.h"

namespace wasm {
namespace {
using ::dart::FLAG_trace_wasm_compilation;  // For use in WasmTrace.
using ::dart::GrowableArray;
using ::dart::Log;  // For use in WasmTrace.
using ::dart::OS;
using ::dart::ReAlloc;
using ::dart::SExpInteger;  // Uses int64_t internally, so it should fit most
                            // serialized Wasm integers.
using ::dart::SExpList;
using ::dart::SExpression;
using ::dart::SExpSymbol;
using ::dart::ValueObject;
using ::dart::WriteStream;
using ::dart::Zone;
}  // namespace

void WasmTrace(const char* format, ...) {
  if (FLAG_trace_wasm_compilation) {
    va_list args;
    va_start(args, format);
    THR_VPrint(format, args);
    va_end(args);
  }
}

// PushBytecountFrontScope is an instance of the RAII programming pattern.
// It is used in the WRITE_BYTECOUNT() macro, which is to only be used
// inside the Output methods of the Wasm classes (i.e. when a WriteStream*
// stream is available for use).
// Example usage and semantics:
//
//  The following *scoped* code inside an Output method:
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
class PushBytecountFrontScope : public ValueObject {
 public:
  // The current WriteStream* is replaced by new stream (and internal buffer)
  // each time the WRITE_BYTECOUNT() macro occurs in a scope. Then,
  // upon leaving the scope, the old stream (and bufer) is restored, with
  // the new contents and their bytecount appended (the latter coming first).
  explicit PushBytecountFrontScope(WriteStream** current_stream_location)
      : replacement_buffer_(nullptr),
        old_stream_location_(current_stream_location),
        old_stream_(*current_stream_location),
        new_stream_(&replacement_buffer_, old_stream_->alloc(), 16) {
    *current_stream_location = &new_stream_;
  }

  ~PushBytecountFrontScope() {
    const intptr_t bytes_written = new_stream_.bytes_written();
    WasmTrace("Copying %" Pd " bytes to parent WriteStream.\n", bytes_written);
    // The Wasm specification treats bytes_written as an unsigned 32 bit integer.
    // While an overflow is technically possible, code of this size is unlikely
    // to occur in practice.
    old_stream_->WriteUnsigned(bytes_written);
    old_stream_->WriteBytes(replacement_buffer_, bytes_written);
    *old_stream_location_ = old_stream_;
  }

 private:
  uint8_t* replacement_buffer_;
  WriteStream** const old_stream_location_;
  WriteStream* const old_stream_;
  WriteStream new_stream_;

  DISALLOW_COPY_AND_ASSIGN(PushBytecountFrontScope);
};

#define WRITE_BYTE(m) stream->WriteFixed(static_cast<uint8_t>(m))
#define WRITE_UNSIGNED(m) stream->WriteUnsigned(m)
#define WRITE_SIGNED(m) stream->Write(m)
// Note: in general, this should be UTF8-compliant, but for our purposes
// here we do not need the non ASCII-cases, so this implementation
// of WRITE_STRING works for the time being.
#define WRITE_STRING(m) stream->WriteBytes(m, strlen(m));

// It is not permitted to have two occurences of this macro in the same
// enclosing scope. This is enforced by a variable name clash if this is
// not respected. This requires users to explicitly open a new scope whenever
// they need multiply-nested PushBytecountFrontScopes, consistent with the
// example uses above.
#define WRITE_BYTECOUNT()                                                      \
  PushBytecountFrontScope push_bytecount_front_scope(&stream)

SExpression* NumType::Serialize(Zone* zone) {
  switch (kind_) {
    case Kind::kI32:
      return new (zone) SExpSymbol("i32");
    case Kind::kI64:
      return new (zone) SExpSymbol("i64");
    case Kind::kF32:
      return new (zone) SExpSymbol("f32");
    case Kind::kF64:
      return new (zone) SExpSymbol("f64");
    case Kind::kV128:
      return new (zone) SExpSymbol("v128");
    default:
      UNREACHABLE();
  }
}

void NumType::OutputBinary(WriteStream* stream) {
  switch (kind_) {
    case Kind::kI32:
      WRITE_BYTE(0x7F);
      break;
    case Kind::kI64:
      WRITE_BYTE(0x7E);
      break;
    case Kind::kF32:
      WRITE_BYTE(0x7D);
      break;
    case Kind::kF64:
      WRITE_BYTE(0x7C);
      break;
    case Kind::kV128:
      WRITE_BYTE(0x7B);
      break;
    default:
      UNREACHABLE();
  }
}

SExpression* RefType::Serialize(Zone* zone) {
  ASSERT(heap_type_ != nullptr);
  // First, try to use the shorthand notations.
  if (!nullable_ && heap_type_->kind_ == HeapType::Kind::kI31) {
    // ref i31 = i31ref.
    return new (zone) SExpSymbol("i31ref");
  } else if (nullable_) {
    // ref null {func/extern/any/eq} = {func/extern/any/eq}ref.
    switch (heap_type_->kind_) {
      case HeapType::Kind::kFunc:
        return new (zone) SExpSymbol("funcref");
      case HeapType::Kind::kExtern:
        return new (zone) SExpSymbol("externref");
      case HeapType::Kind::kAny:
        return new (zone) SExpSymbol("anyref");
      case HeapType::Kind::kEq:
        return new (zone) SExpSymbol("eqref");
      default:
        break;
    }
  }
  // Otherwise, use the general case.
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("ref"));
  if (nullable_) {
    sexp->Add(new (zone) SExpSymbol("null"));
  }
  sexp->Add(heap_type_->Serialize(zone));
  return sexp;
}

void RefType::OutputBinary(WriteStream* stream) {
  ASSERT(heap_type_ != nullptr);
  // First, try to use the shorthand notations.
  if (!nullable_ && heap_type_->kind_ == HeapType::Kind::kI31) {
    // ref i31 = i31ref.
    WRITE_SIGNED(-0x16);
    return;
  } else if (nullable_) {
    // ref null {func/extern/any/eq} = {func/extern/any/eq}ref.
    switch (heap_type_->kind_) {
      case HeapType::Kind::kFunc:
      case HeapType::Kind::kExtern:
      case HeapType::Kind::kAny:
      case HeapType::Kind::kEq:
        heap_type_->OutputBinary(stream);
        return;
      default:
        break;
    }
  }
  // Otherwise, use the general case.
  if (nullable_) {
    WRITE_SIGNED(-0x14);
  } else {
    WRITE_SIGNED(-0x15);
  }
  heap_type_->OutputBinary(stream);
}

SExpression* HeapType::Serialize(Zone* zone) {
  switch (kind_) {
    case Kind::kFunc:
      return new (zone) SExpSymbol("func");
    case Kind::kExtern:
      return new (zone) SExpSymbol("extern");
    case Kind::kTypeidx:
      return new (zone) SExpInteger(def_type_->index());
    case Kind::kAny:
      return new (zone) SExpSymbol("any");
    case Kind::kEq:
      return new (zone) SExpSymbol("eq");
    case Kind::kI31:
      return new (zone) SExpSymbol("i31");
    default:
      UNREACHABLE();
  }
}

void HeapType::OutputBinary(WriteStream* stream) {
  switch (kind_) {
    case Kind::kFunc:
      WRITE_SIGNED(-0x10);
      break;
    case Kind::kExtern:
      WRITE_SIGNED(-0x11);
      break;
    case Kind::kTypeidx:
      WRITE_SIGNED(def_type_->index());
      break;
    case Kind::kAny:
      // https://github.com/v8/v8/blob/master/src/wasm/wasm-constants.h#L36
      FATAL("any/anyref not implemented by V8");
      WRITE_SIGNED(-0x12);
      break;
    case Kind::kEq:
      WRITE_SIGNED(-0x13);
      break;
    case Kind::kI31:
      // Note: https://github.com/WebAssembly/gc/blob/master/proposals/gc/MVP.md
      // lists this as -0x17, but V8 uses -0x16 in their spec and code:
      // - Spec: https://bit.ly/3cWcm6Q
      WRITE_SIGNED(-0x16);
      break;
    default:
      UNREACHABLE();
  }
}

SExpression* Rtt::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new SExpSymbol("rtt"));
  sexp->Add(new SExpInteger(depth_));
  sexp->Add(heap_type_->Serialize(zone));
  return sexp;
}

void Rtt::OutputBinary(WriteStream* stream) {
  WRITE_SIGNED(-0x17);
  WRITE_UNSIGNED(depth_);
  heap_type_->OutputBinary(stream);
}

SExpression* Field::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(field_type_->Serialize(zone));
  sexp->Add(new (zone) SExpSymbol("fieldidx ="));
  sexp->Add(new (zone) SExpInteger(index_));
  return sexp;
}

void Field::OutputBinary(WriteStream* stream) {
  field_type_->OutputBinary(stream);
}

SExpression* FieldType::Serialize(Zone* zone) {
  SExpression* sexp = nullptr;
  switch (packed_type_) {
    case PackedType::kNoType:
      sexp = value_type_->Serialize(zone);
      break;
    case PackedType::kI8:
      sexp = new (zone) SExpSymbol("i8");
      break;
    case PackedType::kI16:
      sexp = new (zone) SExpSymbol("i16");
      break;
    default:
      UNREACHABLE();
  }
  // Add mutable atom if present.
  if (mut_) {
    const auto sexp_list = new (zone) SExpList(zone);
    sexp_list->Add(new (zone) SExpSymbol("mut"));
    sexp_list->Add(sexp);
    return sexp_list;
  } else {
    return sexp;
  }
}

void FieldType::OutputBinary(WriteStream* stream) {
  switch (packed_type_) {
    case PackedType::kNoType:
      value_type_->OutputBinary(stream);
      break;
    case PackedType::kI8:
      WRITE_BYTE(0x7A);
      break;
    case PackedType::kI16:
      WRITE_BYTE(0x79);
      break;
    default:
      UNREACHABLE();
  }
  // Add mutable atom if present.
  if (mut_) {
    WRITE_BYTE(0x01);
  } else {
    WRITE_BYTE(0x00);
  }
}

FuncType::FuncType(Zone* zone, int index)
    : DefType(zone, index), param_types_(zone, 16), result_types_(zone, 16) {}

SExpression* FuncType::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("func"));
  // Add "param" atoms.
  for (ValueType* param_type : param_types_) {
    const auto atom = new (zone) SExpList(zone);
    atom->Add(new (zone) SExpSymbol("param"));
    atom->Add(param_type->Serialize(zone));
    sexp->Add(atom);
  }
  // Add "result" atoms.
  for (ValueType* result_type : result_types_) {
    const auto atom = new (zone) SExpList(zone);
    atom->Add(new (zone) SExpSymbol("result"));
    atom->Add(result_type->Serialize(zone));
    sexp->Add(atom);
  }
  return sexp;
}

void FuncType::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0x60);
  WRITE_UNSIGNED(param_types_.length());
  for (ValueType* type : param_types_) {
    type->OutputBinary(stream);
  }
  WRITE_UNSIGNED(result_types_.length());
  for (ValueType* type : result_types_) {
    type->OutputBinary(stream);
  }
}

void FuncType::AddParam(ValueType* param_type) {
  param_types_.Add(param_type);
}

void FuncType::AddResult(ValueType* result_type) {
  result_types_.Add(result_type);
}

StructType::StructType(Zone* zone, int index)
    : DefType(zone, index), fields_(zone, 16) {}

SExpression* StructType::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("struct"));
  for (Field* field : fields_) {
    sexp->Add(field->Serialize(zone));
  }
  return sexp;
}

void StructType::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0x5F);
  WRITE_UNSIGNED(fields_.length());
  for (Field* field : fields_) {
    field->OutputBinary(stream);
  }
}

void StructType::CopyFieldsTo(StructType* dest) {
  GrowableArray<Field*>& dest_fields = dest->fields();
  for (intptr_t i = 0; i < fields_.length(); ++i) {
    dest_fields.Add(new (zone_) Field(*fields_.At(i)));
    dest_fields.Last()->set_struct_type_(dest);
  }
}

SExpression* ArrayType::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("array"));
  sexp->Add(field_type_->Serialize(zone));
  return sexp;
}

void ArrayType::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0x5E);
  field_type_->OutputBinary(stream);
}

Field* StructType::AddField(FieldType* field_type) {
  fields_.Add(new (zone_) Field(this, field_type, fields_.length()));
  return fields_.Last();
}

Field* StructType::AddField(ValueType* value_type, bool mut) {
  return AddField(new (zone_) FieldType(value_type, mut));
}
Field* StructType::AddField(FieldType::PackedType packed_type, bool mut) {
  return AddField(new (zone_) FieldType(packed_type, mut));
}

SExpression* LocalGet::Serialize(Zone* zone) {
  return new (zone)
      SExpSymbol(OS::SCreate(zone, "local.get %" Pu32, local_->index()));
}

void LocalGet::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0x20);
  WRITE_UNSIGNED(local_->index());
}

SExpression* LocalSet::Serialize(Zone* zone) {
  return new (zone)
      SExpSymbol(OS::SCreate(zone, "local.set %" Pu32, local_->index()));
}

void LocalSet::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0x21);
  WRITE_UNSIGNED(local_->index());
}

SExpression* GlobalGet::Serialize(Zone* zone) {
  return new (zone)
      SExpSymbol(OS::SCreate(zone, "global.get %" Pu32, global_->index()));
}

void GlobalGet::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0x23);
  WRITE_UNSIGNED(global_->index());
}

SExpression* IntOp::Serialize(Zone* zone) {
  uint32_t bits = 0;
  switch (int_kind_) {
    case IntegerKind::kI32:
      bits = 32;
      break;
    case IntegerKind::kI64:
      bits = 64;
      break;
    default:
      UNREACHABLE();
  }
  switch (op_kind_) {
    case OpKind::kAdd:
      return new (zone) SExpSymbol(OS::SCreate(zone, "i%" Pu32 ".add", bits));
    case OpKind::kSub:
      return new (zone) SExpSymbol(OS::SCreate(zone, "i%" Pu32 ".sub", bits));
    case OpKind::kMult:
      return new (zone) SExpSymbol(OS::SCreate(zone, "i%" Pu32 ".mul", bits));
    case OpKind::kDiv:
      return new (zone) SExpSymbol(OS::SCreate(zone, "i%" Pu32 ".div_s", bits));
    case OpKind::kMod:
      return new (zone) SExpSymbol(OS::SCreate(zone, "i%" Pu32 ".mod_s", bits));

    case OpKind::kAnd:
      return new (zone) SExpSymbol(OS::SCreate(zone, "i%" Pu32 ".and", bits));
    case OpKind::kOr:
      return new (zone) SExpSymbol(OS::SCreate(zone, "i%" Pu32 ".or", bits));
    case OpKind::kXor:
      return new (zone) SExpSymbol(OS::SCreate(zone, "i%" Pu32 ".xor", bits));

    case OpKind::kEq:
      return new (zone) SExpSymbol(OS::SCreate(zone, "i%" Pu32 ".eq", bits));
    case OpKind::kNeq:
      return new (zone) SExpSymbol(OS::SCreate(zone, "i%" Pu32 ".ne", bits));
    case OpKind::kLt:
      return new (zone) SExpSymbol(OS::SCreate(zone, "i%" Pu32 ".lt_s", bits));
    case OpKind::kGt:
      return new (zone) SExpSymbol(OS::SCreate(zone, "i%" Pu32 ".gt_s", bits));
    case OpKind::kLe:
      return new (zone) SExpSymbol(OS::SCreate(zone, "i%" Pu32 ".le_s", bits));
    case OpKind::kGe:
      return new (zone) SExpSymbol(OS::SCreate(zone, "i%" Pu32 ".ge_s", bits));
    default:
      UNREACHABLE();
  }
}

void IntOp::OutputBinary(WriteStream* stream) {
  uint32_t offset_arithmetic = 0;
  uint32_t offset_relational = 0;
  if (int_kind_ == IntegerKind::kI64) {
    offset_arithmetic = 0x7C - 0x6A;
    offset_relational = 0x51 - 0x46;
  }
  switch (op_kind_) {
    case OpKind::kAdd:
      WRITE_BYTE(0x6A + offset_arithmetic);
      break;
    case OpKind::kSub:
      WRITE_BYTE(0x6B + offset_arithmetic);
      break;
    case OpKind::kMult:
      WRITE_BYTE(0x6C + offset_arithmetic);
      break;
    case OpKind::kDiv:
      WRITE_BYTE(0x6D + offset_arithmetic);
      break;
    case OpKind::kMod:
      WRITE_BYTE(0x6F + offset_arithmetic);
      break;

    case OpKind::kAnd:
      WRITE_BYTE(0x71 + offset_arithmetic);
      break;
    case OpKind::kOr:
      WRITE_BYTE(0x72 + offset_arithmetic);
      break;
    case OpKind::kXor:
      WRITE_BYTE(0x73 + offset_arithmetic);
      break;

    case OpKind::kEq:
      WRITE_BYTE(0x46 + offset_relational);
      break;
    case OpKind::kNeq:
      WRITE_BYTE(0x47 + offset_relational);
      break;
    case OpKind::kLt:
      WRITE_BYTE(0x48 + offset_relational);
      break;
    case OpKind::kGt:
      WRITE_BYTE(0x4A + offset_relational);
      break;
    case OpKind::kLe:
      WRITE_BYTE(0x4C + offset_relational);
      break;
    case OpKind::kGe:
      WRITE_BYTE(0x4E + offset_relational);
      break;
    default:
      UNREACHABLE();
  }
}

IntOp::OpKind IntOp::NegateOpKind(OpKind op_kind) {
  switch (op_kind) {
    case OpKind::kEq:
      return OpKind::kNeq;
    case OpKind::kNeq:
      return OpKind::kEq;
    case OpKind::kLt:
      return OpKind::kGe;
    case OpKind::kGt:
      return OpKind::kLe;
    case OpKind::kLe:
      return OpKind::kGt;
    case OpKind::kGe:
      return OpKind::kLt;
    default:
      FATAL("Operator can not be negated");
  }
}

SExpression* IntConstant::Serialize(Zone* zone) {
  switch (kind_) {
    case Kind::kI32:
      return new (zone) SExpSymbol(
          OS::SCreate(zone, "i32.const %" Pu32, static_cast<uint32_t>(value_)));
    case Kind::kI64:
      return new (zone)
          SExpSymbol(OS::SCreate(zone, "i64.const %" Pu64, value_));
    default:
      UNREACHABLE();
  }
}

void IntConstant::OutputBinary(WriteStream* stream) {
  switch (kind_) {
    case Kind::kI32:
      WRITE_BYTE(0x41);
      WRITE_SIGNED(static_cast<int32_t>(value_));
      break;
    case Kind::kI64:
      WRITE_BYTE(0x42);
      WRITE_SIGNED(static_cast<int64_t>(value_));
      break;
    default:
      UNREACHABLE();
  }
}

SExpression* Int32WrapInt64::Serialize(Zone* zone) {
  return new (zone) SExpSymbol("i32.wrap_i64");
}

void Int32WrapInt64::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0xA7);
}

Block::Block(FuncType* const block_type, Zone* zone)
    : StructuredInstr(block_type), body_(new (zone) InstructionList(zone)) {}

SExpression* Block::Serialize(Zone* zone) {
  // Serialize result type.
  const auto sexp_type = new (zone) SExpList(zone);
  sexp_type->Add(new SExpSymbol("block_type ="));
  sexp_type->Add(block_type_->Serialize(zone));

  // Serialize body.
  const auto sexp_body = new (zone) SExpList(zone);
  sexp_body->Add(body_->Serialize(zone));

  // Produce final SExpression.
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new SExpSymbol("block"));
  sexp->Add(sexp_type);
  sexp->Add(sexp_body);
  return sexp;
}

void Block::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0x02);
  WRITE_SIGNED(block_type_->index());
  body_->OutputBinary(stream);
  WRITE_BYTE(0x0B);
}

Loop::Loop(FuncType* const block_type, Zone* zone)
    : StructuredInstr(block_type), body_(new (zone) InstructionList(zone)) {}

SExpression* Loop::Serialize(Zone* zone) {
  // Serialize result type.
  const auto sexp_type = new (zone) SExpList(zone);
  sexp_type->Add(new SExpSymbol("block_type ="));
  sexp_type->Add(block_type_->Serialize(zone));

  // Serialize body.
  const auto sexp_body = new (zone) SExpList(zone);
  sexp_body->Add(body_->Serialize(zone));

  // Produce final SExpression.
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new SExpSymbol("loop"));
  sexp->Add(sexp_type);
  sexp->Add(sexp_body);
  return sexp;
}

void Loop::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0x03);
  WRITE_SIGNED(block_type_->index());
  body_->OutputBinary(stream);
  WRITE_BYTE(0x0B);
}

If::If(FuncType* const block_type, Zone* zone)
    : StructuredInstr(block_type),
      then_(new (zone) InstructionList(zone)),
      otherwise_(new (zone) InstructionList(zone)) {}

SExpression* If::Serialize(Zone* zone) {
  // Serialize result type.
  const auto sexp_type = new (zone) SExpList(zone);
  sexp_type->Add(new SExpSymbol("block_type ="));
  sexp_type->Add(block_type_->Serialize(zone));

  // Serialize then branch.
  const auto sexp_then = new (zone) SExpList(zone);
  sexp_then->Add(new SExpSymbol("then"));
  sexp_then->Add(then_->Serialize(zone));

  // Serialize otherwise branch.
  const auto sexp_otherwise = new (zone) SExpList(zone);
  sexp_then->Add(new SExpSymbol("else"));
  sexp_then->Add(otherwise_->Serialize(zone));

  // Produce final SExpression.
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new SExpSymbol("if"));
  sexp->Add(sexp_type);
  sexp->Add(sexp_then);
  sexp->Add(sexp_otherwise);
  return sexp;
}

void If::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0x04);
  WRITE_SIGNED(block_type_->index());
  then_->OutputBinary(stream);
  if (!otherwise_->instructions().is_empty()) {
    WRITE_BYTE(0x05);
    otherwise_->OutputBinary(stream);
  }
  WRITE_BYTE(0x0B);
}

SExpression* Br::Serialize(Zone* zone) {
  if (is_if_) {
    return new (zone) SExpSymbol(OS::SCreate(zone, "br_if %" Pu32, label_));
  } else {
    return new (zone) SExpSymbol(OS::SCreate(zone, "br %" Pu32, label_));
  }
}

void Br::OutputBinary(WriteStream* stream) {
  if (is_if_) {
    WRITE_BYTE(0x0D);
  } else {
    WRITE_BYTE(0x0C);
  }
  WRITE_UNSIGNED(label_);
}

SExpression* RttCanon::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("rtt.canon"));
  sexp->Add(type_->Serialize(zone));
  return sexp;
}

void RttCanon::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0xFB);
  WRITE_BYTE(0x30);
  type_->OutputBinary(stream);
}

SExpression* RttSub::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("rtt.sub"));
  sexp->Add(new (zone) SExpInteger(depth_));
  sexp->Add(supertype_->Serialize(zone));
  sexp->Add(type_->Serialize(zone));
  return sexp;
}

void RttSub::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0xFB);
  WRITE_BYTE(0x31);
  // V8 currently doesn't implement these two immediates.
  // So, we leave them out of our output too. They don't seem neccesary, so
  // it's unclear whether the GC spec will keep them in the long run
  // (see e.g. https://github.com/WebAssembly/function-references/pull/31).
  // WRITE_UNSIGNED(depth_);
  // supertype_->OutputBinary(stream);
  type_->OutputBinary(stream);
}

SExpression* StructGet::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("struct.get"));
  sexp->Add(new (zone) SExpInteger(struct_type_->index()));
  sexp->Add(new (zone) SExpInteger(field_->index()));
  return sexp;
}

void StructGet::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0xFB);
  WRITE_BYTE(0x03);
  WRITE_UNSIGNED(struct_type_->index());
  WRITE_UNSIGNED(field_->index());
}

SExpression* StructSet::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("struct.set"));
  sexp->Add(new (zone) SExpInteger(struct_type_->index()));
  sexp->Add(new (zone) SExpInteger(field_->index()));
  return sexp;
}

void StructSet::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0xFB);
  WRITE_BYTE(0x06);
  WRITE_UNSIGNED(struct_type_->index());
  WRITE_UNSIGNED(field_->index());
}

SExpression* StructNewWithRtt::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  if (def_) {
    sexp->Add(new (zone) SExpSymbol("struct.new_default_with_rtt"));
  } else {
    sexp->Add(new (zone) SExpSymbol("struct.new_with_rtt"));
  }
  sexp->Add(new (zone) SExpInteger(struct_type_->index()));
  return sexp;
}

void StructNewWithRtt::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0xFB);
  if (def_) {
    WRITE_BYTE(0x02);
  } else {
    WRITE_BYTE(0x01);
  }
  WRITE_UNSIGNED(struct_type_->index());
}

SExpression* Call::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("call"));
  sexp->Add(new (zone) SExpInteger(function_->index()));
  return sexp;
}

void Call::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0x10);
  WRITE_UNSIGNED(function_->index());
}

SExpression* CallIndirect::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("call_indirect"));
  sexp->Add(new (zone) SExpInteger(func_type_->index()));
  return sexp;
}

void CallIndirect::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0x11);
  WRITE_UNSIGNED(func_type_->index());
  WRITE_BYTE(0x00);
}

SExpression* Drop::Serialize(Zone* zone) {
  return new (zone) SExpSymbol("drop");
}

void Drop::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0x1A);
}

SExpression* Return::Serialize(Zone* zone) {
  return new (zone) SExpSymbol("return");
}
void Return::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0x0F);
}

SExpression* RefNull::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("ref.null"));
  sexp->Add(type_->Serialize(zone));
  return sexp;
}

void RefNull::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0xD0);
  type_->OutputBinary(stream);
}

SExpression* RefEq::Serialize(Zone* zone) {
  return new (zone) SExpSymbol("ref.eq");
}

void RefEq::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0xD5);
}

SExpression* RefCast::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("ref.cast"));
  sexp->Add(from_type_->Serialize(zone));
  sexp->Add(to_type_->Serialize(zone));
  return sexp;
}

void RefCast::OutputBinary(WriteStream* stream) {
  WRITE_BYTE(0xFB);
  WRITE_BYTE(0x41);
  from_type_->OutputBinary(stream);
  to_type_->OutputBinary(stream);
}

dart::SExpression* Table::Serialize(dart::Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("table"));
  sexp->Add(new (zone) SExpInteger(min_size_));
  sexp->Add(new (zone) SExpInteger(max_size_));
  return sexp;
}

void Table::OutputBinary(dart::WriteStream* stream) {
  WRITE_BYTE(0x70);
  WRITE_BYTE(0x01);
  WRITE_UNSIGNED(min_size_);
  WRITE_UNSIGNED(max_size_);
}

dart::SExpression* SingleElemSegment::Serialize(dart::Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("elem"));
  sexp->Add(new (zone) SExpInteger(offset_));
  sexp->Add(new (zone) SExpInteger(function_->index()));
  return sexp;
}

void SingleElemSegment::OutputBinary(dart::WriteStream* stream) {
  // Table id, will always be zero in our case.
  WRITE_UNSIGNED(0);
  // i32.const offset
  WRITE_BYTE(0x41);
  WRITE_SIGNED(static_cast<int32_t>(offset_));
  WRITE_BYTE(0x0B);
  // Function index.
  WRITE_UNSIGNED(1);
  WRITE_UNSIGNED(function_->index());
}

SExpression* Local::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  switch (kind_) {
    case Kind::kLocal:
      sexp->Add(new (zone) SExpSymbol("local"));
      break;
    case Kind::kParam:
      sexp->Add(new (zone) SExpSymbol("param"));
      break;
    default:
      UNREACHABLE();
  }
  if (strcmp(name_, "") != 0) {
    sexp->Add(new (zone) SExpSymbol(OS::SCreate(zone, "$%s", name_)));
  }
  sexp->Add(type_->Serialize(zone));
  return sexp;
}

void Local::OutputBinary(WriteStream* stream) {
  type_->OutputBinary(stream);
}

Global::Global(Zone* zone, ValueType* type, bool mut, uint32_t index)
    : type_(type),
      mut_(mut),
      init_(new (zone) InstructionList(zone)),
      index_(index) {}

SExpression* Global::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("global"));
  sexp->Add(type_->Serialize(zone));
  if (mut_) {
    sexp->Add(new (zone) SExpSymbol("var"));
  } else {
    sexp->Add(new (zone) SExpSymbol("const"));
  }
  sexp->Add(init_->Serialize(zone));
  return sexp;
}

void Global::OutputBinary(WriteStream* stream) {
  type_->OutputBinary(stream);
  if (mut_) {
    WRITE_BYTE(0x01);
  } else {
    WRITE_BYTE(0x00);
  }
  init_->OutputBinary(stream);
  WRITE_BYTE(0x0B);
}

InstructionList::InstructionList(Zone* zone)
    : zone_(zone), instructions_(zone, 16) {}

SExpression* InstructionList::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  for (Instruction* instr : instructions_) {
    sexp->Add(instr->Serialize(zone));
  }
  return sexp;
}

void InstructionList::OutputBinary(WriteStream* stream) {
  for (Instruction* instr : instructions_) {
    instr->OutputBinary(stream);
  }
}

LocalGet* InstructionList::AddLocalGet(Local* local) {
  LocalGet* const instr = new (zone_) LocalGet(local);
  instructions_.Add(instr);
  return instr;
}

LocalSet* InstructionList::AddLocalSet(Local* local) {
  LocalSet* const instr = new (zone_) LocalSet(local);
  instructions_.Add(instr);
  return instr;
}

GlobalGet* InstructionList::AddGlobalGet(Global* global) {
  GlobalGet* const instr = new (zone_) GlobalGet(global);
  instructions_.Add(instr);
  return instr;
}

IntOp* InstructionList::AddIntOp(IntOp::IntegerKind integer_kind,
                                 IntOp::OpKind op_kind) {
  IntOp* const instr = new (zone_) IntOp(integer_kind, op_kind);
  instructions_.Add(instr);
  return instr;
}

IntConstant* InstructionList::AddI32Constant(uint32_t value) {
  IntConstant* const instr =
      new (zone_) IntConstant(IntConstant::Kind::kI32, value);
  instructions_.Add(instr);
  return instr;
}

IntConstant* InstructionList::AddI64Constant(uint64_t value) {
  IntConstant* const instr =
      new (zone_) IntConstant(IntConstant::Kind::kI64, value);
  instructions_.Add(instr);
  return instr;
}

Int32WrapInt64* InstructionList::AddInt32WrapInt64() {
  Int32WrapInt64* const instr = new (zone_) Int32WrapInt64;
  instructions_.Add(instr);
  return instr;
}

Block* InstructionList::AddBlock(FuncType* block_type) {
  Block* const instr = new (zone_) Block(block_type, zone_);
  instructions_.Add(instr);
  return instr;
}

Loop* InstructionList::AddLoop(FuncType* block_type) {
  Loop* const instr = new (zone_) Loop(block_type, zone_);
  instructions_.Add(instr);
  return instr;
}

If* InstructionList::AddIf(FuncType* block_type) {
  If* const instr = new (zone_) If(block_type, zone_);
  instructions_.Add(instr);
  return instr;
}

RttCanon* InstructionList::AddRttCanon(HeapType* type) {
  RttCanon* const instr = new (zone_) RttCanon(type);
  instructions_.Add(instr);
  return instr;
}

RttSub* InstructionList::AddRttSub(uint32_t depth,
                                   HeapType* supertype,
                                   HeapType* type) {
  RttSub* const instr = new (zone_) RttSub(depth, supertype, type);
  instructions_.Add(instr);
  return instr;
}

StructGet* InstructionList::AddStructGet(StructType* struct_type,
                                         Field* field) {
  StructGet* const instr = new (zone_) StructGet(struct_type, field);
  instructions_.Add(instr);
  return instr;
}

StructSet* InstructionList::AddStructSet(StructType* struct_type,
                                         Field* field) {
  StructSet* const instr = new (zone_) StructSet(struct_type, field);
  instructions_.Add(instr);
  return instr;
}

StructNewWithRtt* InstructionList::AddStructNewWithRtt(
    StructType* struct_type) {
  StructNewWithRtt* const instr =
      new (zone_) StructNewWithRtt(struct_type, /*def =*/false);
  instructions_.Add(instr);
  return instr;
}

StructNewWithRtt* InstructionList::AddStructNewDefaultWithRtt(
    StructType* struct_type) {
  StructNewWithRtt* const instr =
      new (zone_) StructNewWithRtt(struct_type, /*def =*/true);
  instructions_.Add(instr);
  return instr;
}

Call* InstructionList::AddCall(Function* function) {
  Call* const instr = new (zone_) Call(function);
  instructions_.Add(instr);
  return instr;
}

CallIndirect* InstructionList::AddCallIndirect(FuncType* function_type) {
  CallIndirect* const instr = new (zone_) CallIndirect(function_type);
  instructions_.Add(instr);
  return instr;
}

Drop* InstructionList::AddDrop() {
  Drop* const instr = new (zone_) Drop;
  instructions_.Add(instr);
  return instr;
}

Return* InstructionList::AddReturn() {
  Return* const instr = new (zone_) Return();
  instructions_.Add(instr);
  return instr;
}

Br* InstructionList::AddBr(uint32_t label) {
  Br* const instr = new (zone_) Br(/*is_if =*/false, label);
  instructions_.Add(instr);
  return instr;
}

Br* InstructionList::AddBrIf(uint32_t label) {
  Br* const instr = new (zone_) Br(/*is_if =*/true, label);
  instructions_.Add(instr);
  return instr;
}

RefNull* InstructionList::AddRefNull(HeapType* type) {
  RefNull* const instr = new (zone_) RefNull(type);
  instructions_.Add(instr);
  return instr;
}

RefEq* InstructionList::AddRefEq() {
  RefEq* const instr = new (zone_) RefEq();
  instructions_.Add(instr);
  return instr;
}

RefCast* InstructionList::AddRefCast(HeapType* from_type, HeapType* to_type) {
  RefCast* const instr = new (zone_) RefCast(from_type, to_type);
  instructions_.Add(instr);
  return instr;
}

Function::Function(Zone* zone,
                   const char* module_name,
                   const char* name,
                   uint32_t index,
                   FuncType* type)
    : zone_(zone),
      imported_module_name_(module_name),
      name_(name),
      index_(index),
      type_(type),
      locals_(zone, 16),
      body_(nullptr) {}

SExpression* Function::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("func"));
  // Serialize whether the function is imported.
  if (imported_module_name_ != nullptr) {
    sexp->Add(new (zone) SExpSymbol(
        OS::SCreate(zone, "(imported) $%s", imported_module_name_)));
  }
  // Serialize function name.
  if (strcmp(name_, "") != 0) {
    sexp->Add(new (zone) SExpSymbol(OS::SCreate(zone, "$%s", name_)));
  }
  // Serialize type.
  const auto sexp_type = new (zone) SExpList(zone);
  sexp_type->Add(new (zone) SExpSymbol("type"));
  sexp_type->Add(new (zone) SExpInteger(type_->index()));
  sexp->Add(sexp_type);
  // Serialize locals.
  for (Local* it : locals_) {
    if (it->kind() == Local::Kind::kLocal) {
      sexp->Add(it->Serialize(zone));
    }
  }
  // Serialize the body.
  if (body_ != nullptr) {
    sexp->Add(body_->Serialize(zone));
  } else {
    sexp->Add(new (zone) SExpSymbol("<missing body>"));
  }
  return sexp;
}

void Function::OutputBinary(WriteStream* stream) {
  if (IsImported()) {
    FATAL("Imported functions should never be output explicitly");
  }
  // Count locals whose kind is not kParam.
  intptr_t num_non_param_locals = 0;
  for (Local* local : locals_) {
    if (local->kind() == Local::Kind::kLocal) {
      ++num_non_param_locals;
    }
  }
  // First, output the locals.
  WRITE_UNSIGNED(num_non_param_locals);
  for (Local* local : locals_) {
    if (local->kind() == Local::Kind::kLocal) {
      WRITE_BYTE(1);  // One local description follows. Wasm permits
                      // compressing multiple consecutive identical locals
                      // into one. We choose not to use this feature.
      local->OutputBinary(stream);
    }
  }
  // Then, output the function body.
  if (body_ != nullptr) {
    body_->OutputBinary(stream);
  } else {
    WasmTrace("WASM MISSING FUNCTION BODY OMITTED!\n");
  }
  WRITE_BYTE(0x0B);
}

Local* Function::AddLocal(Local::Kind kind, ValueType* type, const char* name) {
  // No further params can be declared after the first
  // local in a Wasm function header.
  ASSERT(kind == Local::Kind::kLocal || locals_.is_empty() ||
         locals_.Last()->kind() == Local::Kind::kParam);
  locals_.Add(new (zone_) Local(this, kind, type, name, locals_.length()));
  return locals_.Last();
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

InstructionList* Function::MakeNewBodyAndClearLocals() {
  locals_.Clear();
  body_ = new (zone_) InstructionList(zone_);
  return body_;
}

WasmModuleBuilder::WasmModuleBuilder(Zone* zone)
    : i32_(NumType::Kind::kI32),
      i64_(NumType::Kind::kI64),
      f32_(NumType::Kind::kF32),
      f64_(NumType::Kind::kF64),
      func_(HeapType::Kind::kFunc),
      ext_(HeapType::Kind::kExtern),
      any_(HeapType::Kind::kAny),
      eq_(HeapType::Kind::kEq),
      i31_(HeapType::Kind::kI31),
      funcref_(/*nullable =*/true, &func_),
      externref_(/*nullable =*/true, &ext_),
      anyref_(/*nullable =*/true, &any_),
      eqref_(/*nullable =*/true, &eq_),
      i31ref_(/*nullable =*/false, &i31_),
      start_function_(nullptr),
      num_imported_functions_(0),
      num_non_imported_functions_(0),
      zone_(zone),
      types_(zone, 16),
      functions_(zone, 16),
      table_(nullptr),
      globals_(zone, 16),
      elem_segments_(zone, 16) {}

SExpression* WasmModuleBuilder::Serialize(Zone* zone) {
  const auto sexp = new (zone) SExpList(zone);
  sexp->Add(new (zone) SExpSymbol("module"));
  // Types section.
  for (DefType* def_type : types_) {
    const auto sexp_type = new (zone) SExpList(zone);
    sexp_type->Add(new (zone) SExpSymbol("type"));
    sexp_type->Add(def_type->Serialize(zone));
    sexp->Add(sexp_type);
  }
  // Table section.
  if (table_ != nullptr) {
    sexp->Add(table_->Serialize(zone));
  }
  // Global section.
  for (Global* global : globals_) {
    sexp->Add(global->Serialize(zone));
  }
  // Start section.
  if (start_function_ != nullptr) {
    const auto sexp_start = new (zone) SExpList(zone);
    sexp_start->Add(new (zone) SExpSymbol("start"));
    sexp_start->Add(new (zone) SExpInteger(start_function_->index()));
    sexp->Add(sexp_start);
  }
  // Element section.
  for (SingleElemSegment* elem : elem_segments_) {
    sexp->Add(elem->Serialize(zone));
  }
  // Import + Function + Code sections.
  // Note that in Wasm Binary Format function bodies would
  // be stored separately, in the code section.
  for (Function* fct : functions_) {
    sexp->Add(fct->Serialize(zone));
  }
  return sexp;
}

void WasmModuleBuilder::OutputTypeSection(WriteStream* stream) {
  // Type section has index 1.
  WRITE_BYTE(1);
  WRITE_BYTECOUNT();
  WRITE_UNSIGNED(types_.length());
  for (DefType* def_type : types_) {
    def_type->OutputBinary(stream);
  }
}

void WasmModuleBuilder::OutputImportSection(WriteStream* stream) {
  // Type section has index 2.
  WRITE_BYTE(2);
  WRITE_BYTECOUNT();
  WRITE_UNSIGNED(num_imported_functions_);
  for (Function* function : functions_) {
    if (function->IsImported()) {
      // Output toplevel namespace name.
      WRITE_UNSIGNED(strlen(function->imported_module_name_));
      WRITE_STRING(function->imported_module_name_);
      // Output imported function name.
      WRITE_UNSIGNED(strlen(function->name_));
      WRITE_STRING(function->name_);
      WRITE_BYTE(0x00);  // Prefix for imported functions.
      WRITE_UNSIGNED(function->type()->index());
    }
  }
}

void WasmModuleBuilder::OutputFunctionSection(WriteStream* stream) {
  // Function section has index 3.
  WRITE_BYTE(3);
  WRITE_BYTECOUNT();
  WRITE_UNSIGNED(num_non_imported_functions_);
  for (Function* function : functions_) {
    if (!function->IsImported()) {
      WRITE_UNSIGNED(function->type()->index());
    }
  }
}

void WasmModuleBuilder::OutputTableSection(dart::WriteStream* stream) {
  // Table section has index 4.
  WRITE_BYTE(4);
  WRITE_BYTECOUNT();
  if (table_ == nullptr) {
    WRITE_UNSIGNED(0);
  } else {
    WRITE_UNSIGNED(1);
    table_->OutputBinary(stream);
  }
}

void WasmModuleBuilder::OutputGlobalSection(WriteStream* stream) {
  // Code section has index 6.
  WRITE_BYTE(6);
  WRITE_BYTECOUNT();
  WRITE_UNSIGNED(globals_.length());
  for (Global* global : globals_) {
    global->OutputBinary(stream);
  }
}

void WasmModuleBuilder::OutputStartSection(WriteStream* stream) {
  if (start_function_ != nullptr) {
    // Start section has index 8.
    WRITE_BYTE(8);
    WRITE_BYTECOUNT();
    WRITE_UNSIGNED(start_function_->index());
  }
}

void WasmModuleBuilder::OutputElementSection(WriteStream* stream) {
  // Element section has index 9.
  WRITE_BYTE(9);
  WRITE_BYTECOUNT();
  WRITE_UNSIGNED(elem_segments_.length());
  for (SingleElemSegment* elem : elem_segments_) {
    elem->OutputBinary(stream);
  }
}

void WasmModuleBuilder::OutputCodeSection(WriteStream* stream) {
  // Code section has index 10.
  WRITE_BYTE(10);
  WRITE_BYTECOUNT();
  WRITE_UNSIGNED(num_non_imported_functions_);
  for (Function* function : functions_) {
    if (!function->IsImported()) {
      // The code of each function is preceded by its bytecount to allow
      // the embedder to lazily compile functions.
      WRITE_BYTECOUNT();
      function->OutputBinary(stream);
    }
  }
}

void WasmModuleBuilder::OutputBinary(WriteStream* stream) {
  ASSERT(stream != nullptr);
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
  // Sections come in ascending order of their indices.
  OutputTypeSection(stream);
  OutputImportSection(stream);
  OutputFunctionSection(stream);
  OutputTableSection(stream);
  OutputGlobalSection(stream);
  OutputStartSection(stream);
  OutputElementSection(stream);
  OutputCodeSection(stream);
}

FieldType* WasmModuleBuilder::MakeFieldType(ValueType* value_type, bool mut) {
  return new (zone_) FieldType(value_type, mut);
}

FieldType* WasmModuleBuilder::MakeFieldType(FieldType::PackedType packed_type,
                                            bool mut) {
  return new (zone_) FieldType(packed_type, mut);
}

ArrayType* WasmModuleBuilder::MakeArrayType(FieldType* field_type) {
  const auto array_type =
      new (zone_) ArrayType(zone_, types_.length(), field_type);
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
  return new (zone_) HeapType(def_type);
}

RefType* WasmModuleBuilder::MakeRefType(bool nullable, HeapType* heap_type) {
  return new (zone_) RefType(nullable, heap_type);
}

RefType* WasmModuleBuilder::MakeRefType(bool nullable, DefType* def_type) {
  return MakeRefType(nullable, MakeHeapType(def_type));
}

Global* WasmModuleBuilder::MakeRttCanon(StructType* type) {
  HeapType* heap_type = MakeHeapType(type);
  Rtt* const rtt = new (zone_) Rtt(1, heap_type);
  Global* const global = AddGlobal(rtt, /*mut =*/false);
  global->init()->AddRttCanon(heap_type);
  return global;
}

Global* WasmModuleBuilder::MakeRttChild(StructType* type,
                                        Global* parent_global) {
  Rtt* const parent_rtt = reinterpret_cast<Rtt*>(parent_global->type_);
  HeapType* heap_type = MakeHeapType(type);
  const intptr_t rtt_depth = 1 + parent_rtt->depth();
  Rtt* const rtt = new (zone_) Rtt(rtt_depth, heap_type);
  Global* const global = AddGlobal(rtt, /*mut =*/false);
  global->init()->AddGlobalGet(parent_global);
  global->init()->AddRttSub(rtt_depth, parent_rtt->heap_type(), heap_type);
  return global;
}

FuncType* WasmModuleBuilder::MakeFuncType() {
  const auto fct_type = new (zone_) FuncType(zone_, types_.length());
  types_.Add(fct_type);
  return fct_type;
}

StructType* WasmModuleBuilder::MakeStructType() {
  const auto str_type = new (zone_) StructType(zone_, types_.length());
  types_.Add(str_type);
  return str_type;
}

Global* WasmModuleBuilder::AddGlobal(ValueType* type, bool mut) {
  Global* global = new (zone_) Global(zone_, type, mut, globals_.length());
  globals_.Add(global);
  return global;
}

Function* WasmModuleBuilder::AddFunction(const char* name, FuncType* type) {
  ++num_non_imported_functions_;
  functions_.Add(new (zone_) Function(zone_, /*module_name=*/nullptr, name,
                                      functions_.length(), type));
  return functions_.Last();
}

Function* WasmModuleBuilder::AddImportedFunction(const char* module_name,
                                                 const char* name,
                                                 FuncType* type) {
  if (!functions_.is_empty() && !functions_.Last()->IsImported()) {
    FATAL("Wasm imported functions need to precede all other functions");
  }
  ++num_imported_functions_;
  functions_.Add(new (zone_) Function(zone_, module_name, name,
                                      functions_.length(), type));
  return functions_.Last();
}

Table* WasmModuleBuilder::AddFunctionsTable(uint32_t min_size,
                                            uint32_t max_size) {
  if (table_ != nullptr) {
    FATAL("Only one table per module can exist.");
  }
  table_ = new (zone_) Table(min_size, max_size);
  return table_;
}

SingleElemSegment* WasmModuleBuilder::AddElemTableInitializer(
    uint32_t offset,
    Function* function) {
  // This restriction shall not neccesarily be enforced, and may be checked
  // later on during module serialization, but it is still good pratice.
  if (table_ == nullptr) {
    FATAL("Table can not be initialized before being declared.");
  }
  SingleElemSegment* const elem_initializer =
      new (zone_) SingleElemSegment(offset, function);
  elem_segments_.Add(elem_initializer);
  return elem_initializer;
}

}  // namespace wasm
