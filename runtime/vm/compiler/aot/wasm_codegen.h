// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef RUNTIME_VM_COMPILER_AOT_WASM_CODEGEN_H_
#define RUNTIME_VM_COMPILER_AOT_WASM_CODEGEN_H_

#include <utility>
#include "vm/compiler/aot/precompiler.h"
#include "vm/compiler/assembler/assembler_wasm.h"
#include "vm/object.h"

namespace dart {

// Forward declarations.
class Precompiler;

struct WasmClassInfo {
  WasmClassInfo() : struct_type_(nullptr), rtt_definition_(nullptr) {}
  WasmClassInfo(wasm::StructType* struct_type, wasm::Global* rtt_definition)
      : struct_type_(struct_type), rtt_definition_(rtt_definition) {}

  // Unforunately, this implicitly needs to be defined for all classes which
  // act as Value components in the Trait of a DirectChainedHashMap
  // template instance.
  bool operator==(const WasmClassInfo& that) {
    return struct_type_ == that.struct_type_ &&
           rtt_definition_ == that.rtt_definition_;
  }
  bool operator!=(const WasmClassInfo& that) {
    return struct_type_ != that.struct_type_ ||
           rtt_definition_ != that.rtt_definition_;
  }

  wasm::StructType* struct_type_;
  wasm::Global* rtt_definition_;
};

struct ClassToWasmTrait {
  // Typedefs needed for the DirectChainedHashMap template.
  typedef const Class* Key;  // Pointer to constant since in some cases
                             // we only have access to const handles.
  typedef WasmClassInfo Value;
  typedef std::pair<Key, Value> Pair;

  static Key KeyOf(Pair kv) { return kv.first; }
  static Value ValueOf(Pair kv) { return kv.second; }
  static intptr_t Hashcode(Key key) { return key->id(); }
  static bool IsKeyEqual(Pair kv, Key key) {
    return kv.first->id() == key->id();
  }
};

struct FunctionToWasmTrait {
  // Typedefs needed for the DirectChainedHashMap template.
  typedef const Function* Key;  // Pointer to constant since in some cases
                                // we only have access to const handles.
  typedef wasm::Function* Value;
  typedef std::pair<Key, Value> Pair;

  static Key KeyOf(Pair kv) { return kv.first; }
  static Value ValueOf(Pair kv) { return kv.second; }
  static intptr_t Hashcode(Key key) { return key->Hash(); }
  static bool IsKeyEqual(Pair kv, Key key) {
    return kv.first->raw() == key->raw();
  }
};

struct FieldToWasmTrait {
  // Typedefs needed for the DirectChainedHashMap template.
  typedef const Field* Key;  // Pointer to constant since in some cases
                             // we only have access to const handles.
  typedef wasm::Field* Value;
  typedef std::pair<Key, Value> Pair;

  static Key KeyOf(Pair kv) { return kv.first; }
  static Value ValueOf(Pair kv) { return kv.second; }
  static intptr_t Hashcode(Key key) {
    const TokenPosition token_pos = key->token_pos();
    if (token_pos.IsReal()) {
      return token_pos.value();
    }
    return key->binary_declaration_offset();
  }
  static bool IsKeyEqual(Pair kv, Key key) {
    return kv.first->raw() == key->raw();
  }
};

class WasmCodegen : public ZoneAllocated {
 public:
  WasmCodegen(Precompiler* precompiler, Zone* zone);

  // Will be used to test new features of the WasmModuleBuilder.
  void Demo();

  // Imports console.log from JS for printing integers.
  void HoistDefaultImports();
  void HoistBuiltinClasses();
  void HoistClassesFromLibrary(const Library& lib);
  // Additionally sets the start function to be "main". Fails with an error
  // if multiple "main" functions have been hoisted.
  void HoistFunctionsFromLibrary(const Library& lib);
  void GenerateClassLayoutsAndRtts();
  // Note that, while code generation for the dispatch table initializers is
  // not known to be faulty, simple cases of such dynamic dispatch are working,
  // whereas more complex usage still fail for an as of yet unknown reason.
  void GenerateWasmDispatchTable(const Array& code_array);

  WasmClassInfo& GetWasmClassInfo(const Class& klass);
  wasm::ValueType* GetWasmType(const Class& type);
  // Note: imported functions will not be found through this method.
  wasm::Function* GetWasmFunction(const Function& function);
  wasm::Field* GetWasmField(const Field& field);

  SExpression* Serialize(Zone* zone);
  void OutputBinary(WriteStream* stream);

  wasm::WasmModuleBuilder* module_builder() { return &module_builder_; }

  wasm::Function* print_i64_func() const { return print_i64_func_; }

  wasm::FuncType* MakeSignature(const Function& function);

  // Helpers for identifying primitive classes.
  // Check if class represents the 'int' class.
  static bool IsIntegerClass(const Class& klass);
  // Check if class represents the 'bool' class.
  static bool IsBoolClass(const Class& klass);
  // Check if class represents the 'String' class.
  static bool IsStringClass(const Class& klass);

  static wasm::Field* GetClassidField(wasm::StructType* wasm_struct) {
    ASSERT(wasm_struct->fields().length() > 0);
    return wasm_struct->fields().At(0);
  }

 private:
  void HoistClass(const Class& klass);
  void HoistFunction(const Function& function);
  void GenerateClassLayoutAndRtt(const Class& klass);
  // Before being compiled in the appropriate compiler pass, functions will
  // possess a dummy signature and have a dummy body implementation.
  // Namely, they will take no arguments and return a hardcoded constant.
  wasm::FuncType* MakeDummySignature();

  Precompiler* const precompiler_;
  Zone* const zone_;
  wasm::WasmModuleBuilder module_builder_;

  // List of hoisted classes.
  GrowableArray<const Class*> classes_;
  // Dart classes are mapped to Wasm structs and rtts.
  DirectChainedHashMap<ClassToWasmTrait> class_to_wasm_class_info_;
  // Dart functions are mapped to Wasm functions.
  DirectChainedHashMap<FunctionToWasmTrait> function_to_wasm_function_;
  // Dart fields are mapped to Wasm struct fields, as follows. First,
  // observe that in Dart fields are not duplicated when a class inherits
  // from another, unlike in Wasm. As a result, a Dart field is mapped
  // to the Wasm field of the class in which occurs originally.
  DirectChainedHashMap<FieldToWasmTrait> field_to_wasm_field_;

  // console.log(i64) imported from JS.
  wasm::Function* print_i64_func_;
  // Wasm value type for the primitive class "Object".
  wasm::ValueType* object_type_;
  // Global rtt definition for the primitive class "Object".
  wasm::Global* object_rtt_;

  DISALLOW_COPY_AND_ASSIGN(WasmCodegen);
};

}  // namespace dart

#endif  // RUNTIME_VM_COMPILER_AOT_WASM_CODEGEN_H_
