// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef RUNTIME_VM_COMPILER_AOT_LLVM_SERIALIZER_H_
#define RUNTIME_VM_COMPILER_AOT_LLVM_SERIALIZER_H_

#include "vm/hash_table.h"
#include "vm/object.h"

namespace dart {

class LLVMStaticCallPatchTraits {
 public:
  static bool ReportStats() { return false; }
  static const char* Name() { return "LLVMStaticCallTraits"; }

  static bool IsMatch(const Object& a, const Object& b) {
    const String& a_str = String::Cast(a);
    const String& b_str = String::Cast(b);

    ASSERT(a_str.HasHash() && b_str.HasHash());
    return a_str.Equals(b_str);
  }

  static uword Hash(const Object& key) { return String::Cast(key).Hash(); }
};

using LLVMStaticCallPatchMap = UnorderedHashMap<LLVMStaticCallPatchTraits>;

class LLVMFunctionsTraits {
 public:
  static const char* Name() { return "LLVMFunctionsTraits"; }
  static bool ReportStats() { return false; }

  // Called when growing the table.
  static bool IsMatch(const Object& a, const Object& b) {
    ASSERT(a.IsFunction() && b.IsFunction());
    // Function objects are always canonical.
    return a.raw() == b.raw();
  }

  static uword Hash(const Object& key) {
    return String::HashRawSymbol(Function::Cast(key).name());
  }
};

using LLVMCompiledFunctionSet = UnorderedHashSet<LLVMFunctionsTraits>;
using LLVMDartFunctionMap = UnorderedHashMap<LLVMFunctionsTraits>;

class LLVMConstantTraits {
 public:
  static bool ReportStats() { return false; }
  static const char* Name() { return "ConstantObjectTraits"; }

  static bool IsMatch(const Object& a, const Object& b) {
    return a.raw() == b.raw();
  }

  static uword Hash(const Object& obj) {
    if (obj.IsNull()) {
      return 2011;
    }
    if (obj.IsString() || obj.IsNumber()) {
      return Instance::Cast(obj).CanonicalizeHash();
    }
    if (obj.IsCode()) {
      // Instructions don't move during compaction.
      return Code::Cast(obj).PayloadStart();
    }
    if (obj.IsFunction()) {
      return Function::Cast(obj).Hash();
    }
    if (obj.IsField()) {
      return dart::String::HashRawSymbol(Field::Cast(obj).name());
    }
    // Unlikely.
    return obj.GetClassId();
  }
};

using LLVMConstantMap = UnorderedHashMap<LLVMConstantTraits>;

class LLVMSerializer : public ValueObject {
 public:
  explicit LLVMSerializer(Thread* thread);
  virtual ~LLVMSerializer();

  void Serialize();

 private:
  Thread* thread() const { return thread_; }
  Isolate* isolate() const { return isolate_; }

  intptr_t AddConstant(const Object& c);
  void AddFunction(const Function& function);
  intptr_t AddDartFunction(const Function& fn);

  void SerializeVMConstants();
  void SerializePatchPoints();
  void SerializeDispatchTable();
  void SerializeAllocationInfo();
  void SerializeDartFunctionDeclarations();

  Thread* thread_;
  Isolate* isolate_;

  intptr_t function_count_ = 0;
  LLVMStaticCallPatchMap static_call_patch_map_;
  LLVMCompiledFunctionSet function_set_;
  LLVMConstantMap constants_;
  LLVMDartFunctionMap dart_compiled_functions_;
  GrowableObjectArray& dart_function_declaration_list_;

  friend class ILSerializer;

  DISALLOW_COPY_AND_ASSIGN(LLVMSerializer);
};

}  // namespace dart

#endif  // RUNTIME_VM_COMPILER_AOT_LLVM_SERIALIZER_H_
