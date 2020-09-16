// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/compiler/aot/wasm_codegen.h"

#define Z (zone_)
#define M (module_builder_)

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

namespace dart {

WasmCodegen::WasmCodegen(Precompiler* precompiler,
                         Zone* zone,
                         uint8_t** binary_output_buffer,
                         ReAlloc realloc)
    : precompiler_(precompiler),
      zone_(zone),
      module_builder_(zone, binary_output_buffer, realloc) {}

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
  fct->AddLocal(wasm::Local::Kind::kParam, M.i32(),
                "x");  // (param $x i32)
  fct->AddLocal(wasm::Local::Kind::kLocal,
                M.MakeRefType(
                    /*nullable =*/true, M.MakeHeapType(str_type)),
                "y");  // (local $y (ref str_type)).
  fct->AddLocal(wasm::Local::Kind::kLocal, M.anyref(),
                "z");  // (local $z anyref).
  fct->AddLocal(wasm::Local::Kind::kLocal, M.i31ref(),
                "t");  // (local $t i31ref).

  // Add instructions to this function: compute 45 + 49.
  wasm::BasicBlock* const block = fct->AddBlock(23);
  block->instructions()->AddConstant(45);
  block->instructions()->AddConstant(49);
  block->instructions()->AddInt32Add();
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
      WASM_TRACE("Hoisting CLASS: %s to Wasm module builder\n",
                 entry.ToCString());
      class_to_wasm_struct_.Insert(std::make_pair(
          &Class::ZoneHandle(Z, Class::Cast(entry).raw()), M.MakeStructType()));
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
        WASM_TRACE("Hoisting METHOD: %s to Wasm module builder\n",
                   function.ToCString());
        HoistFunction(function);
      }
    } else if (entry.IsFunction()) {
      function ^= entry.raw();
      WASM_TRACE("Hoisting FUNCTION: %s to Wasm module builder\n",
                 function.ToCString());
      HoistFunction(function);
    }
  }
}

bool WasmCodegen::IsClassHoisted(const Class& klass) {
  return class_to_wasm_struct_.HasKey(&klass);
}

bool WasmCodegen::IsFunctionHoisted(const Function& function) {
  return function_to_wasm_function_.HasKey(&function);
}

void WasmCodegen::HoistFunction(const Function& function) {
  wasm::FuncType* signature = MakeSignature(function);
  wasm::Function* wasm_function = M.AddFunction(
      String::ZoneHandle(Z, function.name()).ToCString(), signature);
  function_to_wasm_function_.Insert(
      std::make_pair(&Function::ZoneHandle(Z, function.raw()), wasm_function));
}

wasm::FuncType* WasmCodegen::MakeSignature(const Function& function) {
  // TODO(andreicostin): Implement the logic, making sure not to
  // create duplicated function types, for efficiency.
  return M.MakeFuncType(M.i32());  // Placeholder: [] -> [i32]
}

}  // namespace dart