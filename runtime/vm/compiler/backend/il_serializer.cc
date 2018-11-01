// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/compiler/backend/il_serializer.h"

#include "vm/log.h"

namespace dart {

#if !defined(DART_PRECOMPILED_RUNTIME)

namespace {
intptr_t CountInstructions(BlockEntryInstr* instr) {
  intptr_t instr_count = 0;
  ForwardInstructionIterator it(instr);
  for (; !it.Done(); it.Advance()) {
    instr_count += 1 + (it.Current()->IsBranch());
  }
  if (instr->IsBlockEntryWithInitialDefs()) {
    auto initial_defs =
        instr->AsBlockEntryWithInitialDefs()->initial_definitions();
    if (initial_defs != nullptr) {
      instr_count += initial_defs->length();
    }
  }
  if (instr->IsJoinEntry()) {
    auto phis = instr->AsJoinEntry()->phis();
    if (phis != nullptr) {
      instr_count += phis->length();
    }
  }
  return instr_count;
}
}  // namespace

void ILSerializer::VisitInstruction(Instruction* instr) {
  // if (instr->IsDefinition() && instr->AsDefinition()->HasSSATemp()) {
  // TODO(sarkin): Print the v-1 for comparison instructions
  if (instr->IsDefinition()) {
    THR_Print("v%" Pd " ", instr->AsDefinition()->ssa_temp_index());
  }
  instr->Accept(this);
}

void ILSerializer::VisitBlocks() {
  auto& block_order = flow_graph_.reverse_postorder();
  for (BlockEntryInstr* entry : block_order) {
    THR_Print("%" Pd "\n", CountInstructions(entry));
    entry->Accept(this);
    ForwardInstructionIterator it(entry);
    for (; !it.Done(); it.Advance()) {
      auto current = it.Current();
      VisitInstruction(current);
      THR_Print("\n");
    }
    THR_Print("\n");
  }
}

void ILSerializer::Serialize() {
  // TODO(sarkin):
  const String& name = String::Handle(flow_graph_.function().name());
  THR_Print("%s\n", name.ToCString());
  THR_Print("%" Pd32 "\n", flow_graph_.function().IsStaticFunction());
  THR_Print("%" Pd "\n", flow_graph_.function().NumParameters());
  THR_Print("%" Pd "\n", num_blocks_);
  VisitBlocks();
}

void ILSerializer::PrintSerialization(FlowGraph* flow_graph) {
  ILSerializer serializer(*flow_graph);
  serializer.Serialize();
}

void ILSerializer::VisitGraphEntry(GraphEntryInstr* instr) {
  // THR_Print("%" Pd " entry\n", instr->block_id());
  THR_Print("%" Pd "\n", instr->block_id());

  if (instr->initial_definitions() != nullptr) {
    for (Definition* def : *instr->initial_definitions()) {
      VisitInstruction(def);
      THR_Print("\n");
    }
  }
}

void ILSerializer::VisitJoinEntry(JoinEntryInstr* instr) {
  THR_Print("%" Pd "\n", instr->block_id());

  // THR_Print("%" Pd " join ", instr->block_id());
  // for (intptr_t i = 0; i < instr->PredecessorCount(); i++) {
  // THR_Print("%" Pd " ", instr->PredecessorAt(i)->block_id());
  // }
  // THR_Print("\n");

  if (instr->phis() != nullptr) {
    for (PhiInstr* phi : *instr->phis()) {
      VisitInstruction(phi);
      THR_Print("\n");
    }
  }
}

void ILSerializer::VisitTargetEntry(TargetEntryInstr* instr) {
  // THR_Print("%" Pd " target\n", instr->block_id());
  THR_Print("%" Pd "\n", instr->block_id());
}

void ILSerializer::VisitFunctionEntry(FunctionEntryInstr* instr) {
  // THR_Print("%" Pd " function\n", instr->block_id());
  THR_Print("%" Pd "\n", instr->block_id());

  if (instr->initial_definitions() != nullptr) {
    for (Definition* def : *instr->initial_definitions()) {
      VisitInstruction(def);
      THR_Print("\n");
    }
  }
}

void ILSerializer::VisitConstant(ConstantInstr* instr) {
  THR_Print("%s %" Pd, instr->DebugName(), object_order_.Length());
  object_order_.Add(instr->value());
}

void ILSerializer::VisitParameter(ParameterInstr* instr) {
  THR_Print("%s %" Pd, instr->DebugName(), instr->index());
}

void ILSerializer::VisitUnboxInt64(UnboxInt64Instr* instr) {
  THR_Print("%s v%" Pd, instr->DebugName(),
            instr->value()->definition()->ssa_temp_index());
}

void ILSerializer::VisitUnboxedConstant(UnboxedConstantInstr* instr) {
  // TODO(sarkin): Other representations
  auto& value = instr->value();
  if (instr->representation() == kUnboxedInt32 ||
      instr->representation() == kUnboxedInt64) {
    const int64_t val =
        value.IsSmi() ? Smi::Cast(value).Value() : Mint::Cast(value).value();
    THR_Print("%s %" Pd32 " %" Pd64, instr->DebugName(),
              instr->representation(), val);
  } else {
    assert(false);
  }
}

void ILSerializer::VisitPhi(PhiInstr* instr) {
  THR_Print("%s ", instr->DebugName());

  THR_Print("%" Pd32 " ", instr->representation());
  for (intptr_t i = 0; i < instr->InputCount(); i++) {
    THR_Print("v%" Pd " ", instr->InputAt(i)->definition()->ssa_temp_index());
  }
}

void ILSerializer::VisitCheckStackOverflow(CheckStackOverflowInstr* instr) {
  THR_Print("%s", instr->DebugName());
}

void ILSerializer::VisitCheckNull(CheckNullInstr* instr) {
  THR_Print("%s v%" Pd, instr->DebugName(),
            instr->value()->definition()->ssa_temp_index());
}

void ILSerializer::VisitBranch(BranchInstr* instr) {
  VisitInstruction(instr->comparison());
  THR_Print("\n");

  THR_Print("%s ", instr->DebugName());
  THR_Print(" %" Pd " %" Pd, instr->true_successor()->block_id(),
            instr->false_successor()->block_id());
}

void ILSerializer::VisitRelationalOp(RelationalOpInstr* instr) {
  intptr_t operation_cid = instr->operation_cid();
  assert(operation_cid == kSmiCid || operation_cid == kMintCid ||
         operation_cid == kDoubleCid);
  dart_llvm::RelationalOpCid op_cid = (operation_cid == kDoubleCid)
                                          ? dart_llvm::RelationalOpCid::kDouble
                                          : dart_llvm::RelationalOpCid::kInt64;
  THR_Print("%s %" Pd32 " %" Pd32 " ", instr->DebugName(), instr->kind(),
            op_cid);
  THR_Print("v%" Pd " ", instr->left()->definition()->ssa_temp_index());
  THR_Print("v%" Pd, instr->right()->definition()->ssa_temp_index());
}

void ILSerializer::VisitBinaryInt64Op(BinaryInt64OpInstr* instr) {
  THR_Print("%s %" Pd32 " ", instr->DebugName(), instr->op_kind());
  THR_Print("v%" Pd " ", instr->left()->definition()->ssa_temp_index());
  THR_Print("v%" Pd, instr->right()->definition()->ssa_temp_index());
}

void ILSerializer::VisitBoxInt64(BoxInt64Instr* instr) {
  THR_Print("%s v%" Pd, instr->DebugName(),
            instr->value()->definition()->ssa_temp_index());
}

void ILSerializer::VisitGoto(GotoInstr* instr) {
  THR_Print("%s %" Pd, instr->DebugName(), instr->successor()->block_id());
}

void ILSerializer::VisitReturn(ReturnInstr* instr) {
  THR_Print("%s v%" Pd, instr->DebugName(),
            instr->value()->definition()->ssa_temp_index());
}

#endif  // !DART_PRECOMPILED_RUNTIME

}  // namespace dart
