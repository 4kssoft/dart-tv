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
      print_i64_func_(nullptr),
      object_type_(nullptr),
      object_rtt_(nullptr) {}

void WasmCodegen::Demo() {
  // Make a Wasm struct type {i32, mut i64}.
  wasm::StructType* const str_type = M.MakeStructType();
  str_type->AddField(M.i32(), /*mut =*/false);
  str_type->AddField(M.i64(), /*mut =*/true);

  // Make a Wasm function type [i64 f32 f64] -> [i32].
  wasm::FuncType* const fct_type = M.MakeFuncType();
  fct_type->AddParam(M.i64());
  fct_type->AddParam(M.f32());
  fct_type->AddParam(M.f64());
  fct_type->AddResult(M.i32());

  // Make a Wasm function $fct, of type [i64 f32 f64] -> [i32].
  wasm::Function* const fct = M.AddFunction("fct", fct_type);
  wasm::InstructionList* const instrs = fct->MakeNewBodyAndClearLocals();

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
  instrs->AddI32Constant(45);
  instrs->AddI32Constant(49);
  instrs->AddIntOp(wasm::IntOp::IntegerKind::kI32, wasm::IntOp::OpKind::kAdd);
}

void WasmCodegen::HoistDefaultImports() {
  wasm::FuncType* signature = M.MakeFuncType();
  signature->AddParam(M.i64());
  print_i64_func_ = M.AddImportedFunction("console", "log", signature);
}

void WasmCodegen::HoistBuiltinClasses() {
  // Make an "Object" class.
  HoistClass(Class::Handle(Type::Handle(Type::ObjectType()).type_class()));
  // Make a "String" class.
  HoistClass(Class::Handle(Type::Handle(Type::StringType()).type_class()));
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

void WasmCodegen::GenerateWasmDispatchTable(const Array& code_array) {
  HANDLESCOPE(Thread::Current());

  // Add table to module.
  M.AddFunctionsTable(code_array.Length(), code_array.Length());

  // Iterate through elements in dispatch table and initialize the Wasm table.
  auto& code = Code::Handle();
  auto& function = Function::Handle();
  for (intptr_t i = 0; i < code_array.Length(); i++) {
    code = Code::RawCast(code_array.At(i));
    if (code.IsNull()) {
      continue;
    }
    if (!code.IsFunctionCode()) {
      continue;
    }
    function = code.function();

    ASSERT(!function.IsNull());
    wasm::Function* const wasm_function = GetWasmFunction(function);
    if (wasm_function == nullptr) continue;

    M.AddElemTableInitializer(i, wasm_function);
    WasmTrace("Wasm dispatch table has function %" Pu32 " at position %" Pd
              "\n",
              wasm_function->index(), i);
  }
}

void WasmCodegen::GenerateClassLayoutsAndRtts() {
  HANDLESCOPE(Thread::Current());
  for (intptr_t i = 0; i < classes_.length(); ++i) {
    const Class& klass = *classes_.At(i);
    GenerateClassLayoutAndRtt(klass);
  }
}

WasmClassInfo& WasmCodegen::GetWasmClassInfo(const Class& klass) {
  pair<const Class*, WasmClassInfo>* const pair =
      class_to_wasm_class_info_.Lookup(&klass);
  // At this point all classes should have been hoisted.
  // If this is not true, exit with an error even in non-debug builds.
  if (pair == nullptr) {
    FATAL1("WasmCodegen::GetWasmClassInfo called on non-hoisted class %s",
           klass.ToCString());
  }
  return pair->second;
}

wasm::ValueType* WasmCodegen::GetWasmType(const Class& klass) {
  // Integers and booleans are always unboxed in this implementation.
  if (IsIntegerClass(klass)) {
    return M.i64();
  }
  if (IsBoolClass(klass)) {
    return M.i32();
  }

  wasm::StructType* const wasm_struct = GetWasmClassInfo(klass).struct_type_;
  // At this point all classes which appear as fields should have been
  // hoisted. If this is not true, exit with an error even in non-debug
  // builds.
  if (wasm_struct == nullptr) {
    FATAL1("missing for %s", klass.ToCString());
  }
  return M.MakeRefType(/*nullable =*/true, wasm_struct);
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
  wasm::FuncType* signature = MakeDummySignature();
  const String& function_name = String::ZoneHandle(Z, function.name());
  wasm::Function* wasm_function =
      M.AddFunction(function_name.ToCString(), signature);

  if (strcmp(function_name.ToCString(), "main") == 0) {
    if (M.start_function() != nullptr) {
      FATAL("Multiple main functions detected");
    }
    M.set_start_function(wasm_function);
  }

  // TODO(andreicostin): Normally, we would leave hoisted functions bodyless
  // until the Wasm compiler pass compiles a body for them, but for demo
  // purposes, we'll just create a demo body for them which is consistent
  // with the dummy signature defined below ([] -> [i64]).
  wasm_function->MakeNewBodyAndClearLocals()->AddI64Constant(100);

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
    object_type_ = M.MakeRefType(/*nullable =*/true, wasm_struct);
    // For class id.
    wasm_struct->AddField(M.i32(), /*mut =*/true);
    // Rtt - root of class hierarchy.
    object_rtt_ = M.MakeRttCanon(wasm_struct);
    wasm_rtt = object_rtt_;
    return;
  }
  // Special handling for Strings. Largely unimplemented for now.
  if (IsStringClass(klass)) {
    // For class id.
    wasm_struct->AddField(M.i32(), /*mut =*/true);
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

    wasm::ValueType* const field_type = GetWasmType(type_class);
    wasm::Field* const wasm_field = wasm_struct->AddField(field_type,
                                                          /*mut =*/true);
    field_to_wasm_field_.Insert(
        make_pair(&Field::ZoneHandle(Z, field.raw()), wasm_field));
  }
}

wasm::FuncType* WasmCodegen::MakeDummySignature() {
  wasm::FuncType* const func_type = M.MakeFuncType();
  func_type->AddResult(M.i64());
  return func_type;
}

wasm::FuncType* WasmCodegen::MakeSignature(const Function& function) {
  const intptr_t num_params = function.NumParameters();
  const AbstractType& result_type =
      AbstractType::Handle(function.result_type());
  const Class& result_type_class = Class::Handle(result_type.type_class());
  const String& function_name = String::Handle(function.name());
  wasm::FuncType* const func_type = M.MakeFuncType();
  // The main function should have signature "void main()" in our implementation.
  if (strcmp(function_name.ToCString(), "main") == 0) {
    if (num_params > 0 || !result_type.IsVoidType()) {
      FATAL("Dart main function should have signature void main()");
    }
    return func_type;
  }

  // Translate function arguments.
  Type& type = Type::Handle();
  Class& type_class = Class::Handle();
  for (intptr_t i = 0; i < num_params; ++i) {
    type ^= function.ParameterTypeAt(i);
    type_class ^= type.type_class();
    if (function.HasThisParameter() && i == 0) {
      // Due to Wasm requiring a type for call_indirect which is at least
      // as general as all possible receiver function types at an instance call
      // site, we choose (for simplicity) to not specialize the type of the
      // "this" argument, but rather downcast in the prelude of each function.
      func_type->AddParam(object_type_);
    } else {
      func_type->AddParam(GetWasmType(type_class));
    }
  }
  // Translate return value.
  // A Dart return value of "Void" should translate
  // into no return value in Wasm.
  WasmTrace("Return value WasmTrace for function %s has returned type %s",
            function.ToCString(), result_type_class.ToCString());
  if (!result_type_class.IsVoidClass()) {
    func_type->AddResult(GetWasmType(result_type_class));
  }
  return func_type;
}

bool WasmCodegen::IsIntegerClass(const Class& klass) {
  return klass.raw() == Type::Handle(Type::IntType()).type_class() ||
         IsIntegerClassId(klass.id());
}

bool WasmCodegen::IsBoolClass(const Class& klass) {
  return klass.raw() == Type::Handle(Type::BoolType()).type_class();
}

bool WasmCodegen::IsStringClass(const Class& klass) {
  return klass.raw() == Type::Handle(Type::StringType()).type_class() ||
         IsStringClassId(klass.id());
}

}  // namespace dart
