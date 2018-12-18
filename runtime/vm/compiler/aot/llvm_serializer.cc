// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/compiler/aot/llvm_serializer.h"

#include "vm/compiler/backend/il_serializer.h"
#include "vm/hash_table.h"
#include "vm/object.h"
#include "vm/object_store.h"

namespace dart {

#define T (thread())
#define I (isolate())

#if !defined(DART_PRECOMPILED_RUNTIME)

LLVMSerializer::LLVMSerializer(Thread* thread)
    : thread_(thread),
      isolate_(thread->isolate()),
      static_call_patch_map_(
          HashTables::New<LLVMStaticCallPatchMap>(0, Heap::kNew)),
      function_set_(HashTables::New<LLVMCompiledFunctionSet>(0, Heap::kNew)),
      constants_(HashTables::New<LLVMConstantMap>(0, Heap::kOld)),
      dart_compiled_functions_(
          HashTables::New<LLVMDartFunctionMap>(0, Heap::kOld)),
      dart_function_declaration_list_(GrowableObjectArray::Handle(
          I->object_store()->dart_function_declaration_list())) {
  constants_.InsertOrGetValue(Bool::False(), Smi::Handle(Smi::New(0)));
  constants_.InsertOrGetValue(Bool::True(), Smi::Handle(Smi::New(1)));
  constants_.InsertOrGetValue(Object::null_object(), Smi::Handle(Smi::New(2)));
}

LLVMSerializer::~LLVMSerializer() {
  static_call_patch_map_.Release();
  function_set_.Release();
  I->object_store()->set_llvm_constants(constants_.Release());
  I->object_store()->set_dart_compiled_functions(
      dart_compiled_functions_.Release());
}

void LLVMSerializer::AddFunction(const Function& function) {
  function_set_.Insert(function);
  THR_Print("%" Pd "\n", function_count_);
  const GrowableObjectArray& llvm_compiled_functions =
      GrowableObjectArray::Handle(I->object_store()->llvm_compiled_functions());
  llvm_compiled_functions.Add(function);
  function.set_llvm_id(function_count_);
  function_count_++;
}

void LLVMSerializer::Serialize() {
  ASSERT(function_count_ > 0);
  {
    // False, True, Null offsets
    THR_Print("0\n");
    THR_Print("1\n");
    THR_Print("2\n");

    THR_Print("%" Pd "\n", constants_.NumOccupied());
  }

  SerializeVMConstants();
  SerializePatchPoints();
  SerializeDispatchTable();
  SerializeAllocationInfo();
  SerializeDartFunctionDeclarations();

  THR_Print("%" Pd "\n", function_count_);
}

void LLVMSerializer::SerializeVMConstants() {
  auto P = [&](auto v) { THR_Print("%" Pd "\n", v); };
  auto P32 = [&](auto v) { THR_Print("%" Pd32 "\n", v); };

  P32(kWordSize);
  P32(kWordSizeLog2);
  P32(kNewObjectBitPosition);
  P32(kObjectAlignment);
  P32(kObjectAlignmentLog2);

  P(Thread::top_exit_frame_info_offset());
  P(Thread::top_offset());
  P(Thread::end_offset());
  P(Thread::predefined_symbols_address_offset());
  P(Thread::store_buffer_block_offset());
  P(Thread::marking_stack_block_offset());
  P(Thread::write_barrier_code_offset());
  P(Thread::write_barrier_mask_offset());
  P(Thread::stack_limit_offset());

  P32(StoreBufferBlock::kSize);
  P(StoreBufferBlock::top_offset());
  P(StoreBufferBlock::pointers_offset());

  P32(MarkingStackBlock::kSize);
  P(MarkingStackBlock::top_offset());
  P(MarkingStackBlock::pointers_offset());

  P32(RawObject::kOldAndNotRememberedBit);
  P32(RawObject::kOldAndNotMarkedBit);
  P(RawObject::kBarrierOverlapShift);

  P(Object::tags_offset());

  // Same as Object::tags_offset, serialize for consistency.
  P(Instance::tags_offset());
  P(Instance::NextFieldOffset());

  P(Field::static_value_offset());
  P(Field::guarded_cid_offset());
  P(Field::is_nullable_offset());
  P(Field::kind_bits_offset());
  P32(Field::kUnboxingCandidateBit);

  P32(Symbols::kNullCharCodeSymbolOffset);

  P(Array::kMaxNewSpaceElements);
  P(sizeof(RawArray));
  P(RawObject::SizeTag::kMaxSizeTag);
  P32(RawObject::kSizeTagPos);
  {
    uint32_t tags = 0;
    tags = RawObject::ClassIdTag::update(kArrayCid, tags);
    tags = RawObject::NewBit::update(true, tags);
    P32(tags);
  }
  P(Array::tags_offset());
  P(Array::type_arguments_offset());
  P(Array::length_offset());

  P32(kSmiCid);
  P32(kMintCid);
  P32(kDoubleCid);
  P32(kFloat32x4Cid);
  P32(kFloat64x2Cid);
  P32(kNullCid);
  P32(kBoolCid);
  P32(kArrayCid);
  P32(kImmutableArrayCid);

  // For LoadIndexed/StoreIndexed
  P32(kTypedDataFloat32ArrayCid);
  P32(kTypedDataFloat64ArrayCid);
  P32(kTypedDataInt8ArrayCid);
  P32(kTypedDataUint8ArrayCid);
  P32(kTypedDataUint8ClampedArrayCid);
  P32(kExternalTypedDataUint8ArrayCid);
  P32(kExternalTypedDataUint8ClampedArrayCid);
  P32(kOneByteStringCid);
  P32(kExternalOneByteStringCid);
  P32(kTypedDataInt16ArrayCid);
  P32(kTypedDataUint16ArrayCid);
  P32(kTypedDataInt32ArrayCid);
  P32(kTypedDataUint32ArrayCid);
  P32(kTypedDataInt64ArrayCid);
  P32(kTypedDataUint64ArrayCid);
  P32(kTwoByteStringCid);
  P32(kExternalTwoByteStringCid);

  P(Mint::value_offset());
  P(Double::value_offset());
  P(Float32x4::value_offset());
  P(Float64x2::value_offset());
}

void LLVMSerializer::SerializePatchPoints() {
  THR_Print("%" Pd "\n", static_call_patch_map_.NumOccupied());
  // Should I add a handlescope here?
  LLVMStaticCallPatchMap::Iterator it(&static_call_patch_map_);
  String& patch_point = String::Handle();
  while (it.MoveNext()) {
    intptr_t entry = it.Current();
    patch_point ^= static_call_patch_map_.GetKey(entry);
    Function& fn = Function::Handle();
    fn ^= static_call_patch_map_.GetOrDie(patch_point);

    // TODO(sarkin): Use the compiled_function_set.
    auto is_llvm_compiled = [&] { return patch_point.ToCString()[0] == '1'; };
    intptr_t id_to_print = (is_llvm_compiled()) ? fn.llvm_id() : fn.dart_id();
    THR_Print("%s\n%" Pd "\n", patch_point.ToCString(), id_to_print);
  }
}

void LLVMSerializer::SerializeDispatchTable() {
  // Leave all the heavy-lifting to the standalone tool: for each class
  // list all its functions and the functions of its superclasses in that
  // order.
  auto class_table = I->class_table();
  auto& obj = Object::Handle();
  auto& cls = Class::Handle();
  auto& scls = Class::Handle();
  auto& funcs = Array::Handle();
  auto& func = Function::Handle();
  auto& func_name = String::Handle();

  auto add_dart_function = [&](const Function& fn) {
    auto map_size = dart_compiled_functions_.NumOccupied();
    auto smi_offset = dart_compiled_functions_.InsertOrGetValue(
        fn, Smi::Handle(Smi::New(map_size)));
    auto offset = Smi::Value(Smi::RawCast(smi_offset));

    if (offset == map_size) {
      dart_function_declaration_list_.Add(fn);
    }
    fn.set_dart_id(offset);
  };

  THR_Print("%" Pd "\n", class_table->NumCids());
  for (intptr_t cid = 0; cid < class_table->NumCids(); ++cid) {
    if (!class_table->IsValidIndex(cid) || !class_table->HasValidClassAt(cid)) {
      THR_Print("-1\n");
      continue;
    }
    cls ^= class_table->At(cid);
    scls ^= cls.SuperClass();
    intptr_t scid = (scls.IsNull()) ? -1 : scls.id();
    THR_Print("%" Pd " %" Pd " ", cid, scid);
    for (; !cls.IsNull(); cls = cls.SuperClass()) {
      auto serialize_funcs = [&]() {
        if (funcs.IsNull()) {
          return;
        }
        for (intptr_t i = 0; i < funcs.Length(); ++i) {
          obj = funcs.At(i);
          if (!obj.IsFunction()) {
            continue;
          }
          func ^= funcs.At(i);
          func_name ^= func.name();
          obj = function_set_.GetOrNull(func);
          bool is_llvm = !obj.IsNull();

          if (!is_llvm) {
            add_dart_function(func);
          }
          auto id = (is_llvm) ? func.llvm_id() : func.dart_id();
          THR_Print("%s %" Pd32 " %" Pd " ", func_name.ToCString(), is_llvm,
                    id);
        }
      };
      funcs ^= cls.functions();
      serialize_funcs();
      funcs ^= cls.invocation_dispatcher_cache();
      serialize_funcs();
    }
    THR_Print("\n");
  }
}

void LLVMSerializer::SerializeDartFunctionDeclarations() {
  THR_Print("%" Pd "\n", dart_function_declaration_list_.Length());
  auto& fn = Function::Handle();
  for (intptr_t i = 0; i < dart_function_declaration_list_.Length(); ++i) {
    fn ^= dart_function_declaration_list_.At(i);
    THR_Print("%" Pd "\n", i);
    ILSerializer::SerializeFunctionDeclaration(fn);
  }
}

void LLVMSerializer::SerializeAllocationInfo() {
  auto class_table = I->class_table();
  auto& cls = Class::Handle();
  for (intptr_t cid = 0; cid < class_table->NumCids(); ++cid) {
    if (!class_table->IsValidIndex(cid) || !class_table->HasValidClassAt(cid)) {
      THR_Print("-1\n");
      continue;
    }
    cls ^= class_table->At(cid);
    if (!cls.is_type_finalized()) {
      THR_Print("-1\n");
      continue;
    }
    const bool is_cls_parameterized = cls.NumTypeArguments() > 0;
    const intptr_t instance_size = cls.instance_size();
    const bool is_allocatable_in_new_space =
        Heap::IsAllocatableInNewSpace(instance_size);
    const bool has_fast_path =
        is_allocatable_in_new_space && !cls.TraceAllocation(I);
    uint32_t tags = 0;
    tags = RawObject::SizeTag::update(instance_size, tags);
    ASSERT(cls.id() != kIllegalCid);
    tags = RawObject::ClassIdTag::update(cls.id(), tags);
    tags = RawObject::NewBit::update(true, tags);
    // TODO(sarkin): Serializing all valid classes results
    // in a crash when deserializing the app snapshot.
    // For now, only serialize the classes needed for box allocation.
    auto should_serialize_cls = [&](size_t cid) {
      return cid == kMintCid || cid == kDoubleCid;
    };
    THR_Print("%" Pd " ", (should_serialize_cls(cid)) ? AddConstant(cls) : -1);
    THR_Print("%" Pd32 " ", is_cls_parameterized);
    THR_Print("%" Pd " ", instance_size);
    THR_Print("%" Pd32 " ", is_allocatable_in_new_space);
    THR_Print("%" Pd32 " ", has_fast_path);
    THR_Print("%" Pd32 " ", tags);
    THR_Print("%" Pd "\n", cls.type_arguments_field_offset());
  }
}

intptr_t LLVMSerializer::AddConstant(const Object& c) {
  auto map_size = constants_.NumOccupied();
  auto smi_offset =
      constants_.InsertOrGetValue(c, Smi::Handle(Smi::New(map_size)));
  auto offset = Smi::Value(Smi::RawCast(smi_offset));
  return offset;
}

intptr_t LLVMSerializer::AddDartFunction(const Function& fn) {
  auto map_size = dart_compiled_functions_.NumOccupied();
  auto smi_offset = dart_compiled_functions_.InsertOrGetValue(
      fn, Smi::Handle(Smi::New(map_size)));
  auto offset = Smi::Value(Smi::RawCast(smi_offset));

  if (offset == map_size) {
    dart_function_declaration_list_.Add(fn);
  }

  return offset;
}

#endif  // !defined(DART_PRECOMPILED_RUNTIME)

}  // namespace dart
