// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef RUNTIME_VM_COMPILER_AOT_WASM_CODEGEN_H_
#define RUNTIME_VM_COMPILER_AOT_WASM_CODEGEN_H_

#include "utility"
#include "vm/compiler/aot/precompiler.h"
#include "vm/compiler/assembler/assembler_wasm.h"
#include "vm/object.h"

namespace dart {

// Forward declarations.
class Precompiler;

struct ClassToWasmTrait {
  // Typedefs needed for the DirectChainedHashMap template.
  typedef const Class* Key;  // Pointer to constant since in some cases
                             // we only have access to const handles.
  typedef wasm::StructType* Value;
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

class WasmCodegen : public ZoneAllocated {
 public:
  WasmCodegen(Precompiler* precompiler, Zone* zone);

  // Will be used to test new features of the WasmModuleBuilder.
  void Demo();

  void HoistClassesFromLibrary(const Library& lib);
  void HoistFunctionsFromLibrary(const Library& lib);

  wasm::StructType* GetWasmClass(const Class& klass);
  wasm::Function* GetWasmFunction(const Function& function);

  SExpression* Serialize(Zone* zone);
  void OutputBinary(WriteStream* stream);

 private:
  void HoistClass(const Class& klass);
  void HoistFunction(const Function& function);
  wasm::FuncType* MakeSignature(const Function& function);

  Precompiler* const precompiler_;
  Zone* const zone_;
  wasm::WasmModuleBuilder module_builder_;

  DirectChainedHashMap<ClassToWasmTrait> class_to_wasm_struct_;
  DirectChainedHashMap<FunctionToWasmTrait> function_to_wasm_function_;

  DISALLOW_COPY_AND_ASSIGN(WasmCodegen);
};

}  // namespace dart

#endif  // RUNTIME_VM_COMPILER_AOT_WASM_CODEGEN_H_
