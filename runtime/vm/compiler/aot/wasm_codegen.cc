// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/compiler/aot/wasm_codegen.h"

#define Z (zone_)
#define M (module_builder_)

namespace dart {
namespace {
using ::std::make_pair;
using ::std::pair;
using ::wasm::WasmTrace;
}  // namespace

WasmCodegen::WasmCodegen(Precompiler* precompiler, Zone* zone)
    : precompiler_(precompiler),
      zone_(zone),
      module_builder_(zone),
      classes_(zone, 16),
      class_to_wasm_class_info_(zone),
      function_to_wasm_function_(zone),
      field_to_wasm_field_(zone),
      object_rtt_(nullptr) {}

void WasmCodegen::Demo() {
  // Make a Wasm struct type {i32, mut i64}.
  wasm::StructType* const str_type = M.MakeStructType();
  str_type->AddField(M.i32(), /*mut =*/false);
  str_type->AddField(M.i64(), /*mut =*/true);

  // Make a Wasm function type [i64 f32 f64] -> [i32].
  wasm::FuncType* const fct_type = M.MakeFuncType(M.i32());
  fct_type->AddParam(M.i64());
  fct_type->AddParam(M.f32());
  fct_type->AddParam(M.f64());

  // Make a Wasm function $fct, of type [i64 f32 f64] -> [i32].
  wasm::Function* const fct = M.AddFunction("fct", fct_type);

  // Register four locals to this function.
  // Note: Parameters are *not* checked against func_type.
  // So, this function is not well-formed, but it doesn't matter
  // for testing purposes.
  // fct->AddLocal(wasm::Local::Kind::kParam, M.i32(),
  //               "x");  // (param $x i32)

  fct->AddLocal(wasm::Local::Kind::kLocal,
                M.MakeRefType(
                    /*nullable =*/true, M.MakeHeapType(str_type)),
                "y1");  // (local $y (ref null str_type)).

  // Note: Non-nullable reference local variables crash V8.
  // Bug reported to jkummerow.
  // fct->AddLocal(wasm::Local::Kind::kLocal,
  //               M.MakeRefType(
  //                   /*nullable =*/false, M.MakeHeapType(str_type)),
  //               "y2");  // (local $y (ref str_type)).

  // Note: V8 doesn't support any/anyref yet.
  // fct->AddLocal(wasm::Local::Kind::kLocal, M.anyref(),
  //               "t0");  // (local $t1 anyref).

  // Tests serialization/binary output for reference types.
  fct->AddLocal(wasm::Local::Kind::kLocal, M.eqref(),
                "t1");  // (local $t1 eqref).
  // fct->AddLocal(wasm::Local::Kind::kLocal, M.i31ref(),
  //               "t2");  // (local $t2 i31ref).
  // fct->AddLocal(wasm::Local::Kind::kLocal,
  //               M.MakeRefType(/*nullable = */ false, M.eq()),
  //               "q1");  // (local $q1 (ref eq)).
  fct->AddLocal(wasm::Local::Kind::kLocal, M.funcref(),
                "t2");  // (local $t2 funcref).
  fct->AddLocal(wasm::Local::Kind::kLocal, M.externref(),
                "t3");  // (local $t3 externref).
  fct->AddLocal(wasm::Local::Kind::kLocal,
                M.MakeRefType(/*nullable = */ true, M.i31()),
                "t4");  // (local $t4 (ref null i31)).

  // Add instructions to this function: compute 45 + 49.
  wasm::InstructionList* const instrs = fct->MakeNewBody();
  instrs->AddConstant(45);
  instrs->AddConstant(49);
  instrs->AddInt32Add();
}

void WasmCodegen::HoistClassesFromLibrary(const Library& lib) {
  HANDLESCOPE(Thread::Current());
  const Array& dict = Array::Handle(lib.dictionary());
  const intptr_t dict_size = dict.Length() - 1;  // To skip the top level class.
  Object& entry = Object::Handle();
  for (intptr_t i = 0; i < dict_size; ++i) {
    entry = dict.At(i);
    if (entry.IsNull()) {
      continue;
    }
    if (entry.IsClass()) {
      HoistClass(Class::Cast(entry));
    }
  }
}

void WasmCodegen::HoistFunctionsFromLibrary(const Library& lib) {
  HANDLESCOPE(Thread::Current());
  const Array& dict = Array::Handle(lib.dictionary());
  const intptr_t dict_size = dict.Length() - 1;  // To skip the top level class.
  Object& entry = Object::Handle();
  Function& function = Function::Handle();
  Array& functions = Array::Handle();
  for (intptr_t i = 0; i < dict_size; ++i) {
    entry = dict.At(i);
    if (entry.IsNull()) {
      continue;
    }
    if (entry.IsClass()) {
      functions ^= Class::Cast(entry).functions();
      if (functions.IsNull()) {
        continue;
      }
      for (intptr_t j = 0; j < functions.Length(); j++) {
        function ^= functions.At(j);
        if (function.IsNull()) {
          continue;
        }
        WasmTrace("Hoisting METHOD: %s to Wasm module builder\n",
                  function.ToCString());
        HoistFunction(function);
      }
    } else if (entry.IsFunction()) {
      function ^= entry.raw();
      WasmTrace("Hoisting FUNCTION: %s to Wasm module builder\n",
                function.ToCString());
      HoistFunction(function);
    }
  }
}

void WasmCodegen::GenerateClassLayoutsAndRtts() {
  HANDLESCOPE(Thread::Current());
  for (intptr_t i = 0; i < classes_.length(); ++i) {
    const Class& klass = *classes_.At(i);
    GenerateClassLayoutAndRtt(klass);
  }
}

void WasmCodegen::HoistBuiltinClasses() {
  // Make an "Object" class.
  HoistClass(Class::Handle(Type::Handle(Type::ObjectType()).type_class()));
  // Make an "int" class.
  HoistClass(Class::Handle(Type::Handle(Type::IntType()).type_class()));
  // Make a "String" class.
  HoistClass(Class::Handle(Type::Handle(Type::StringType()).type_class()));
}

WasmClassInfo& WasmCodegen::GetWasmClassInfo(const Class& klass) {
  pair<const Class*, WasmClassInfo>* const pair =
      class_to_wasm_class_info_.Lookup(&klass);
  // At this point all classes should have been hoisted.
  // If this is not true, exit with an error even in non-debug builds.
  RELEASE_ASSERT(pair != nullptr);
  return pair->second;
}

wasm::Function* WasmCodegen::GetWasmFunction(const Function& function) {
  return function_to_wasm_function_.LookupValue(&function);
}

wasm::Field* WasmCodegen::GetWasmField(const Field& field) {
  return field_to_wasm_field_.LookupValue(&field);
}

SExpression* WasmCodegen::Serialize(Zone* zone) {
  return module_builder_.Serialize(zone);
}

void WasmCodegen::OutputBinary(WriteStream* stream) {
  module_builder_.OutputBinary(stream);
}

void WasmCodegen::HoistClass(const Class& klass) {
  WasmTrace("Hoisting CLASS: %s to Wasm module builder, with cid = %" Pd "\n",
            klass.ToCString(), klass.id());
  wasm::StructType* wasm_struct = M.MakeStructType();
  const Class& persistent_class = Class::ZoneHandle(Z, klass.raw());
  classes_.Add(&persistent_class);
  class_to_wasm_class_info_.Insert(
      make_pair(&persistent_class, WasmClassInfo(wasm_struct, nullptr)));
}

void WasmCodegen::HoistFunction(const Function& function) {
  wasm::FuncType* signature = MakeSignature(function);
  wasm::Function* wasm_function = M.AddFunction(
      String::ZoneHandle(Z, function.name()).ToCString(), signature);

  // TODO(andreicostin): Normally, we would leave hoisted functions bodyless
  // until the Wasm compiler pass compiles a body for them, but for demo
  // purposes, we'll just create a demo body for them which is consistent
  // with the dummy signature defined below ([] -> [i32]).
  wasm_function->MakeNewBody()->AddConstant(100);

  function_to_wasm_function_.Insert(
      make_pair(&Function::ZoneHandle(Z, function.raw()), wasm_function));
}

void WasmCodegen::GenerateClassLayoutAndRtt(const Class& klass) {
  if (klass.IsNull()) {
    FATAL("GenerateClassLayoutAndRtt called with Null class");
  }
  WasmTrace("GenerateClassLayoutAndRtt reached class %s, with cid = %" Pd "\n",
            klass.ToCString(), klass.id());

  WasmClassInfo& wasm_class_info = GetWasmClassInfo(klass);
  wasm::StructType* const wasm_struct = wasm_class_info.struct_type_;
  wasm::Global*& wasm_rtt = wasm_class_info.rtt_definition_;

  // Memoization - if class has already had its layout generated, exit.
  if (!wasm_struct->fields().is_empty()) {
    return;
  }

  // Rtts shouldn't have been allocated before the struct
  // layout has been computed.
  RELEASE_ASSERT(wasm_rtt == nullptr);

  WasmTrace("Generating Wasm layout and rtt for class %s, with cid = %" Pd "\n",
            klass.ToCString(), klass.id());

  // Special handling for root class "Object".
  if (klass.IsObjectClass()) {
    // For class id.
    wasm_struct->AddField(M.i64(), /*mut =*/true);
    // Rtt - root of class hierarchy.
    RELEASE_ASSERT(wasm_struct != nullptr);  // todo remove.
    object_rtt_ = M.MakeRttCanon(wasm_struct);
    wasm_rtt = object_rtt_;
    return;
  }
  // Special handling for integers. In the future integers will translated
  // into either this struct (when boxed) or i64 (when unboxed).
  if (IsIntegerClass(klass)) {
    // For class id.
    wasm_struct->AddField(M.i64(), /*mut =*/true);
    // Contained integer.
    wasm_struct->AddField(M.i64(), /*mut =*/true);
    // Rtt - hardcode parent to be "Object", despite the real
    // parent being "Num".
    wasm_rtt = M.MakeRttChild(wasm_struct, object_rtt_);
    return;
  }
  // Special handling for Strings. Largely unimplemented for now.
  if (IsStringClass(klass)) {
    // For class id.
    wasm_struct->AddField(M.i64(), /*mut =*/true);
    // Rtt - parent is "Object".
    wasm_rtt = M.MakeRttChild(wasm_struct, object_rtt_);
    return;
  }

  const Class& parent_class = Class::Handle(klass.SuperClass());

  // Ensure that the parent class has had its layout and rtt generated
  // before continuing.
  GenerateClassLayoutAndRtt(parent_class);

  // Get parent struct and rtt.
  WasmClassInfo& parent_wasm_class_info = GetWasmClassInfo(parent_class);
  wasm::StructType* const parent_wasm_struct =
      parent_wasm_class_info.struct_type_;
  // Parent struct should have been allocated.
  if (parent_wasm_struct == nullptr) {
    FATAL1("parent_wasm_struct missing for parent class %s",
           parent_class.ToCString());
  }
  wasm::Global* const parent_wasm_rtt = parent_wasm_class_info.rtt_definition_;
  // Parent rtt should have been allocated.
  if (parent_wasm_rtt == nullptr) {
    FATAL1("parent_wasm_rtt missing for parent class %s",
           parent_class.ToCString());
  }

  // Allocate rtt.
  wasm_rtt = M.MakeRttChild(wasm_struct, parent_wasm_rtt);

  // Allocate struct layout.
  parent_wasm_struct->CopyFieldsTo(wasm_struct);
  const Array& fields = Array::Handle(klass.fields());
  Field& field = Field::Handle();
  Type& type = Type::Handle();
  Class& type_class = Class::Handle();
  for (intptr_t i = 0; i < fields.Length(); ++i) {
    field ^= fields.At(i);
    type ^= field.type();
    type_class = type.type_class();

    WasmTrace("--> Processing field %s\n", field.ToCString());

    wasm::StructType* const field_wasm_struct =
        GetWasmClassInfo(type_class).struct_type_;
    // At this point all classes which appear as fileds should have been
    // hoisted. If this is not true, exit with an error even in non-debug
    // builds.
    if (field_wasm_struct == nullptr) {
      FATAL1("field_wasm_struct missing for %s", field.ToCString());
    }

    wasm::Field* const wasm_field = wasm_struct->AddField(
        M.MakeRefType(/*nullable =*/true, field_wasm_struct),
        /*mut =*/true);

    field_to_wasm_field_.Insert(
        make_pair(&Field::ZoneHandle(Z, field.raw()), wasm_field));
  }
}

wasm::FuncType* WasmCodegen::MakeSignature(const Function& function) {
  // TODO(andreicostin): Implement the logic, making sure not to
  // create duplicated function types, for efficiency.
  return M.MakeFuncType(M.i32());  // Placeholder: [] -> [i32]
}

bool WasmCodegen::IsIntegerClass(const Class& klass) {
  return klass.raw() == Type::Handle(Type::IntType()).type_class() ||
         IsIntegerClassId(klass.id());
}

bool WasmCodegen::IsStringClass(const Class& klass) {
  return klass.raw() == Type::Handle(Type::StringType()).type_class() ||
         IsStringClassId(klass.id());
}

}  // namespace dart