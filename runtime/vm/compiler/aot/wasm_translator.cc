// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/compiler/aot/wasm_translator.h"

void WasmTranslator::PrepareBlocks() {
  // Find start positions for Wasm blocks.
  // The Wasm block for each forward edge is started immediately inside the
  // innermost loop containing the target block. The Wasm blocks belonging
  // to the same loop are started in opposite order of their target blocks,
  // such that they can each be ended at their target block.
  // Wasm blocks for forward edges with targets outside of all loops are
  // started immediately at the start of the body.
  dart::GrowableArray<BlockEntryInstr*> forward_stack;
  for (dart::BlockIterator block_it = flow_graph_->postorder_iterator();
       !block_it.Done(); block_it.Advance()) {
    BlockEntryInstr* const block = block_it.Current();
    intptr_t block_number = block->postorder_number();
    if (auto join = block->AsJoinEntry()) {
      bool fallthrough_edge = false;
      bool forward_edge = false;
      bool backward_edge = false;
      intptr_t forward_count = 0;
      for (intptr_t i = 0; i < join->PredecessorCount(); i++) {
        BlockEntryInstr* pred = join->PredecessorAt(i);
        intptr_t pred_number = pred->postorder_number();
        if (pred_number > block_number + 1) {
          forward_edge = true;
        } else if (pred_number <= block_number) {
          backward_edge = true;
          // Mark start of all Wasm blocks corresponding to forward edges with
          // targets inside this loop.
          while (!forward_stack.is_empty() &&
                 forward_stack.Last()->postorder_number() >= pred_number) {
            edge_stack_.Add(forward_stack.RemoveLast());
            forward_count++;
          }
        } else {
          fallthrough_edge = true;
        }
      }
      ASSERT(fallthrough_edge);
      if (forward_edge) {
        forward_stack.Add(block);
      }
      if (backward_edge) {
        edge_stack_.Add(block);
        forward_count_stack_.Add(forward_count);
      }
    } else if (auto target = block->AsTargetEntry()) {
      BlockEntryInstr* pred = target->PredecessorAt(0);
      intptr_t pred_number = pred->postorder_number();
      ASSERT(pred_number > block_number);
      if (pred_number > block_number + 1) {
        forward_stack.Add(block);
      }
    }
  }

  // Start Wasm blocks with targets outside of all loops.
  for (BlockEntryInstr* target : forward_stack) {
    StartWasmBlock(target);
  }
}

void WasmTranslator::VisitJoinEntry(JoinEntryInstr* block) {
  bool is_forward_target =
      !scope_stack_.is_empty() && scope_stack_.Last() == block;
  bool is_backward_target =
      !edge_stack_.is_empty() && edge_stack_.Last() == block;
  if (is_forward_target) {
    EndWasmBlock(/* pop_phi_values = */ !is_backward_target);
  }
  if (is_backward_target) {
    StartWasmLoop(/* push_phi_values = */ !is_forward_target);
  }
}

void WasmTranslator::VisitTargetEntry(TargetEntryInstr* block) {
  bool is_forward_target =
      !scope_stack_.is_empty() && scope_stack_.Last() == block;
  if (is_forward_target) {
    EndWasmBlock(/* pop_phi_values = */ false);
  }
}

void WasmTranslator::VisitGoto(GotoInstr* instr) {
  BlockEntryInstr* source = instr->block();
  intptr_t source_number = source->postorder_number();
  JoinEntryInstr* target = instr->successor();
  intptr_t target_number = target->postorder_number();
  if (target_number >= source_number) {
    // Backward edge.
    if (scope_stack_.Last() != target) {
      irreducible_ = true;
      return;
    }
    EndWasmLoop(source);
  } else if (target_number < source_number - 1) {
    // Forward edge.
    PushPhiValues(target, GetSourcePredecessorIndex(target, source));
    intptr_t label = GetLabelForTarget(target);
    if (label == -1) {
      irreducible_ = true;
      return;
    }
    EmitWasmBranch(label);
  }
}

void WasmTranslator::VisitBranch(BranchInstr* instr) {
  BlockEntryInstr* source = instr->GetBlock();
  intptr_t source_number = source->postorder_number();
  TargetEntryInstr* target;
  bool negated;
  if (instr->true_successor()->postorder_number() == source_number - 1) {
    // Branch on false, fall through on true.
    target = instr->false_successor();
    negated = true;
  } else {
    // Branch on true, fall through on false.
    ASSERT(instr->false_successor()->postorder_number() == source_number - 1);
    target = instr->true_successor();
    negated = false;
  }
  // TODO(andreicostin): Emit code for computing the result of
  // instr->comparison() and negate it if negate is true.
  // Push the resulting condition on the stack as an i32.
  EmitWasmBranchIf(GetLabelForTarget(target));
}

intptr_t WasmTranslator::GetFallthroughPredecessorIndex(
    BlockEntryInstr* target) {
  intptr_t block_number = target->postorder_number();
  for (intptr_t i = 0; i < target->PredecessorCount(); i++) {
    BlockEntryInstr* pred = target->PredecessorAt(i);
    intptr_t pred_number = pred->postorder_number();
    if (pred_number == block_number + 1) {
      return i;
    }
  }
  return -1;
}

intptr_t WasmTranslator::GetSourcePredecessorIndex(BlockEntryInstr* target,
                                                   BlockEntryInstr* source) {
  for (intptr_t i = 0; i < target->PredecessorCount(); i++) {
    BlockEntryInstr* pred = target->PredecessorAt(i);
    if (pred == source) {
      return i;
    }
  }
  UNREACHABLE();
}

intptr_t WasmTranslator::GetLabelForTarget(BlockEntryInstr* target) {
  for (intptr_t label = 0; label < scope_stack_.length(); label++) {
    if (scope_stack_[scope_stack_.length() - 1 - label] == target) {
      return label;
    }
  }
  return -1;
}

void WasmTranslator::PushPhiValues(JoinEntryInstr* join, intptr_t pred_index) {
  if (join->phis() != nullptr) {
    for (intptr_t p = 0; p < join->phis()->length(); p++) {
      PushValue(join->phis()->At(p)->InputAt(pred_index)->definition());
    }
  }
}

void WasmTranslator::PopPhiValues(JoinEntryInstr* join) {
  if (join->phis() != nullptr) {
    // Accept phi values (in reverse order).
    for (intptr_t p = join->phis()->length() - 1; p >= 0; p--) {
      PopValue(join->phis()->At(p));
    }
  }
}

void WasmTranslator::PushValue(Definition* def) {
  // TODO(andreicostin): Push local correponding to def on Wasm stack.
}

void WasmTranslator::PopValue(Definition* def) {
  // TODO(andreicostin): Pop local correponding to def from Wasm stack.
}

void WasmTranslator::EmitWasmBranch(intptr_t label) {
  // TODO(andreicostin): Emit br Wasm instruction with the given label index.
}

void WasmTranslator::EmitWasmBranchIf(intptr_t label) {
  // TODO(andreicostin): Emit br_if Wasm instruction with the given label index.
}

void WasmTranslator::StartWasmBlock(BlockEntryInstr* target) {
  scope_stack_.Add(target);
  // TODO(andreicostin): Start Wasm block with output types matching the phi
  // nodes of target.
}

void WasmTranslator::EndWasmBlock(bool pop_phi_values) {
  BlockEntryInstr* target = scope_stack_.RemoveLast();
  if (auto join = target->AsJoinEntry()) {
    intptr_t pred_index = GetFallthroughPredecessorIndex(target);
    if (pred_index != -1) {
      // Fall-through edge - push phi values
      PushPhiValues(join, pred_index);
    }
  }
  // TODO(andreicostin): End current Wasm block.
  if (pop_phi_values) {
    if (auto join = target->AsJoinEntry()) {
      PopPhiValues(join);
    }
  }
}

void WasmTranslator::StartWasmLoop(bool push_phi_values) {
  JoinEntryInstr* target = edge_stack_.RemoveLast()->AsJoinEntry();
  if (push_phi_values) {
    intptr_t pred_index = GetFallthroughPredecessorIndex(target);
    ASSERT(pred_index != -1);
    PushPhiValues(target, pred_index);
  }
  scope_stack_.Add(target);
  // TODO(andreicostin): Start Wasm loop with input types matching the phi
  // nodes of target.
  PopPhiValues(target);
  // Start all Wasm blocks corresponding to forward edges with targets inside
  // this loop.
  intptr_t forward_count = forward_count_stack_.RemoveLast();
  while (forward_count-- > 0) {
    StartWasmBlock(edge_stack_.RemoveLast());
  }
}

void WasmTranslator::EndWasmLoop(BlockEntryInstr* source) {
  JoinEntryInstr* target = scope_stack_.RemoveLast()->AsJoinEntry();
  PushPhiValues(target, GetSourcePredecessorIndex(target, source));
  // Branch to label index 0 (since the loop is currently the innermost scope).
  EmitWasmBranch(0);
  // TODO(andreicostin): End current Wasm loop.
}
