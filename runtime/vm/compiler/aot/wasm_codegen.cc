// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/compiler/aot/wasm_codegen.h"

#define Z (zone_)
#define M (module_builder_)

namespace dart {
namespace {
using ::wasm::WasmTrace;
}  // namespace

WasmCodegen::WasmCodegen(Precompiler* precompiler, Zone* zone)
    : precompiler_(precompiler), zone_(zone), module_builder_(zone) {}

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
      WasmTrace("Hoisting CLASS: %s to Wasm module builder\n",
                entry.ToCString());
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

wasm::StructType* WasmCodegen::GetWasmClass(const Class& klass) {
  return class_to_wasm_struct_.LookupValue(&klass);
}

wasm::Function* WasmCodegen::GetWasmFunction(const Function& function) {
  return function_to_wasm_function_.LookupValue(&function);
}

SExpression* WasmCodegen::Serialize(Zone* zone) {
  return module_builder_.Serialize(zone);
}

void WasmCodegen::OutputBinary(WriteStream* stream) {
  module_builder_.OutputBinary(stream);
}

void WasmCodegen::HoistClass(const Class& klass) {
  wasm::StructType* wasm_struct = M.MakeStructType();
  wasm_struct->AddField(M.i64(), /*mut =*/false);  // For class id.
  class_to_wasm_struct_.Insert(
      std::make_pair(&Class::ZoneHandle(Z, klass.raw()), wasm_struct));
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
      std::make_pair(&Function::ZoneHandle(Z, function.raw()), wasm_function));
}

wasm::FuncType* WasmCodegen::MakeSignature(const Function& function) {
  // TODO(andreicostin): Implement the logic, making sure not to
  // create duplicated function types, for efficiency.
  return M.MakeFuncType(M.i32());  // Placeholder: [] -> [i32]
}

}  // namespace dart