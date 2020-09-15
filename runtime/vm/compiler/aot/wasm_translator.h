// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef RUNTIME_VM_COMPILER_AOT_WASM_TRANSLATOR_H_
#define RUNTIME_VM_COMPILER_AOT_WASM_TRANSLATOR_H_

#include "vm/compiler/backend/flow_graph.h"
#include "vm/compiler/backend/il.h"
#include "vm/growable_array.h"

using dart::BlockEntryInstr;
using dart::BranchInstr;
using dart::Definition;
using dart::GotoInstr;
using dart::JoinEntryInstr;
using dart::TargetEntryInstr;

class WasmTranslator : public dart::FlowGraphVisitor {
 public:
  explicit WasmTranslator(dart::FlowGraph* flow_graph)
      : FlowGraphVisitor(flow_graph->reverse_postorder()),
        flow_graph_(flow_graph),
        irreducible_(false) {}

  void Translate() {
    PrepareBlocks();
    VisitBlocks();
  }

  bool irreducible() { return irreducible_; }

  virtual void VisitJoinEntry(JoinEntryInstr* block);
  virtual void VisitTargetEntry(TargetEntryInstr* block);
  virtual void VisitGoto(GotoInstr* instr);
  virtual void VisitBranch(BranchInstr* instr);

 private:
  // Traverse graph to find start locations of Wasm blocks.
  void PrepareBlocks();

  intptr_t GetFallthroughPredecessorIndex(BlockEntryInstr* target);
  intptr_t GetSourcePredecessorIndex(BlockEntryInstr* target,
                                     BlockEntryInstr* source);
  intptr_t GetLabelForTarget(BlockEntryInstr* target);

  void PushPhiValues(JoinEntryInstr* join, intptr_t pred_index);
  void PopPhiValues(JoinEntryInstr* join);

  void PushValue(Definition* def);
  void PopValue(Definition* def);
  void EmitWasmBranch(intptr_t label);
  void EmitWasmBranchIf(intptr_t label);

  void StartWasmBlock(BlockEntryInstr* target);
  void EndWasmBlock(bool pop_phi_values);
  void StartWasmLoop(bool push_phi_values);
  void EndWasmLoop(BlockEntryInstr* source);

  dart::FlowGraph* flow_graph_;

  // Target blocks of forward and backward edges, sorted (reversed) by the
  // order in which the corresponding Wasm block or loop should be started.
  dart::GrowableArray<BlockEntryInstr*> edge_stack_;
  // For each backward edge on the edge stack, how many Wasm blocks should be
  // started right inside the loop block (because their corresponding targets
  // reside inside the loop).
  dart::GrowableArray<intptr_t> forward_count_stack_;
  // The target blocks of the current stack of Wasm scopes. The label index
  // for a forward jump can be found by searching from the top for the target.
  dart::GrowableArray<BlockEntryInstr*> scope_stack_;

  // Did the transformation fail because the graph is not reducible?
  bool irreducible_;
};

#endif  // RUNTIME_VM_COMPILER_AOT_WASM_TRANSLATOR_H_
