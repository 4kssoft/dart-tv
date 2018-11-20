// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef RUNTIME_VM_COMPILER_BACKEND_IL_SERIALIZER_H_
#define RUNTIME_VM_COMPILER_BACKEND_IL_SERIALIZER_H_

#include "vm/compiler/aot/precompiler.h"
#include "vm/compiler/backend/flow_graph.h"
#include "vm/compiler/backend/il.h"
#include "vm/compiler/backend/llvm_common_defs.h"

namespace dart {

class LLVMSerializer;

#if !defined(DART_PRECOMPILED_RUNTIME)
class ILSerializer : public FlowGraphVisitor {
 public:
  explicit ILSerializer(const FlowGraph& flow_graph,
                        LLVMSerializer* llvm_serializer);
  virtual ~ILSerializer() {}

#define DECLARE_VISIT_INSTRUCTION(ShortName, Attrs)                            \
  virtual void Visit##ShortName(ShortName##Instr* instr);
  FOR_EACH_SUPPORTED_INSTRUCTION(DECLARE_VISIT_INSTRUCTION)
#undef DECLARE_VISIT_INSTRUCTION

  void VisitInstruction(Instruction* instr);

  virtual void VisitBlocks();

  void Serialize();

  static void SerializeFunctionDeclaration(const Function& function);

  static void PrintSerialization(FlowGraph* flow_graph,
                                 LLVMSerializer* llvm_serializer);

 private:
  intptr_t AddConstant(const Object& c) {
    return llvm_serializer_->AddConstant(c);
  }

  void AddFunction(const Function& function) {
    llvm_serializer_->AddFunction(function);
  }

  intptr_t AddDartFunction(const Function& fn) {
    return llvm_serializer_->AddDartFunction(fn);
  }

  void SerializeValue(Value* val) const;

  void SerializeArgumentsDescriptor(const ArgumentsDescriptor& arg_desc) const;
  void SerializeInstanceCall(InstanceCallInstr* instr,
                             bool check_smi,
                             bool serialize_arguments = true);
  void SerializeCall(const TemplateDartCall<0>* instr,
                     bool serialize_arguments = true);

  void SerializeIndexOrConstant(Definition* d) const;
  template <typename T>
  void SerializeCompare(T instr) const;

  intptr_t static_call_count_ = 0;
  intptr_t num_blocks_ = 0;
  JoinEntryInstr* current_join_ = nullptr;
  const FlowGraph& flow_graph_;
  LLVMSerializer* llvm_serializer_ = nullptr;
  DISALLOW_COPY_AND_ASSIGN(ILSerializer);
};
#endif  // !DART_PRECOMPILED_RUNTIME

}  // namespace dart

#endif  // RUNTIME_VM_COMPILER_BACKEND_IL_SERIALIZER_H_
