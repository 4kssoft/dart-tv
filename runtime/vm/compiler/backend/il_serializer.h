// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef RUNTIME_VM_COMPILER_BACKEND_IL_SERIALIZER_H_
#define RUNTIME_VM_COMPILER_BACKEND_IL_SERIALIZER_H_

#include "platform/text_buffer.h"
#include "vm/compiler/backend/flow_graph.h"
#include "vm/compiler/backend/il.h"
#include "vm/compiler/backend/llvm_common_defs.h"
#include "vm/object_store.h"

namespace dart {

#if !defined(DART_PRECOMPILED_RUNTIME)
class ILSerializer : public FlowGraphVisitor {
 public:
  explicit ILSerializer(const FlowGraph& flow_graph)
      : FlowGraphVisitor(flow_graph.reverse_postorder()),
        num_blocks_(flow_graph.reverse_postorder().length()),
        flow_graph_(flow_graph),
        object_order_(GrowableObjectArray::Handle(
            Isolate::Current()
                ->object_store()
                ->il_serialization_object_order())) {}

#define DECLARE_VISIT_INSTRUCTION(ShortName, Attrs)                            \
  virtual void Visit##ShortName(ShortName##Instr* instr);
  FOR_EACH_SUPPORTED_INSTRUCTION(DECLARE_VISIT_INSTRUCTION)
#undef DECLARE_VISIT_INSTRUCTION

  void VisitInstruction(Instruction* instr);

  virtual void VisitBlocks();

  void Serialize();

  static void PrintSerialization(FlowGraph* flow_graph);

 private:
  intptr_t num_blocks_;
  const FlowGraph& flow_graph_;
  GrowableObjectArray& object_order_;
  DISALLOW_COPY_AND_ASSIGN(ILSerializer);
};
#endif  // !DART_PRECOMPILED_RUNTIME

}  // namespace dart

#endif  // RUNTIME_VM_COMPILER_BACKEND_IL_SERIALIZER_H_
