// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/compiler/aot/wasm_translator.h"

#define C (codegen_)
#define M (C->module_builder())

namespace dart {
namespace {
using ::wasm::WasmTrace;
}  // namespace

void WasmTranslator::PrepareBlocks() {
  // Find start positions for Wasm blocks.
  // The Wasm block for each forward edge is started immediately inside the
  // innermost loop containing the target block. The Wasm blocks belonging
  // to the same loop are started in opposite order of their target blocks,
  // such that they can each be ended at their target block.
  // Wasm blocks for forward edges with targets outside of all loops are
  // started immediately at the start of the body.
  GrowableArray<BlockEntryInstr*> forward_stack;
  for (BlockIterator block_it = flow_graph_->postorder_iterator();
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
  PushEvalCondition(instr->comparison(), negated);
  EmitWasmBranchIf(GetLabelForTarget(target));
}

void WasmTranslator::VisitBinaryIntegerOp(BinaryIntegerOpInstr* instr) {
  if (instr->InputCount() != 2) {
    FATAL("Unexpected IL input count for comparison.");
  }
  wasm::IntOp::OpKind op_kind;
  switch (instr->op_kind()) {
    case Token::Kind::kADD:
      op_kind = wasm::IntOp::OpKind::kAdd;
      break;
    case Token::Kind::kSUB:
      op_kind = wasm::IntOp::OpKind::kSub;
      break;
    case Token::Kind::kMUL:
      op_kind = wasm::IntOp::OpKind::kMult;
      break;
    case Token::Kind::kTRUNCDIV:
      op_kind = wasm::IntOp::OpKind::kDiv;
      break;
    case Token::Kind::kMOD:
      op_kind = wasm::IntOp::OpKind::kMod;
      break;
    case Token::Kind::kBIT_AND:
      op_kind = wasm::IntOp::OpKind::kAnd;
      break;
    case Token::Kind::kBIT_OR:
      op_kind = wasm::IntOp::OpKind::kOr;
      break;
    case Token::Kind::kBIT_XOR:
      op_kind = wasm::IntOp::OpKind::kXor;
      break;
    default:
      FATAL("BinaryIntegerOp with unexpected token kind.");
  }
  PushValue(instr->InputAt(0)->definition());
  PushValue(instr->InputAt(1)->definition());
  wasm_scope_->AddIntOp(wasm::IntOp::IntegerKind::kI64, op_kind);
  PopValue(instr);
}

void WasmTranslator::VisitBinaryInt32Op(BinaryInt32OpInstr* instr) {
  VisitBinaryIntegerOp(instr);
}

void WasmTranslator::VisitBinaryInt64Op(BinaryInt64OpInstr* instr) {
  VisitBinaryIntegerOp(instr);
}

void WasmTranslator::VisitBinaryUint32Op(BinaryUint32OpInstr* instr) {
  // TODO(andreicostin): This needs to use the unsigned operation kinds.
  // It will be correct in all cases which don't use overflow explicitly.
  VisitBinaryIntegerOp(instr);
}

void WasmTranslator::VisitStaticCall(StaticCallInstr* instr) {
  const intptr_t num_params = instr->ArgumentCount();
  const Function& function = instr->function();
  wasm::Function* wasm_function = C->GetWasmFunction(function);
  if (wasm_function == nullptr) {
    // If the function has not been found, check if it's the print function.
    const AbstractType& result_type =
        AbstractType::Handle(function.result_type());
    const String& function_name = String::Handle(function.name());
    if (strcmp(function_name.ToCString(), "print") == 0) {
      // TODO(andreicostin): Currently missing the check that the
      // first type argument is Object.
      if (num_params != 1 || !result_type.IsVoidType()) {
        FATAL(
            "Dart print function should have signature void print(Object arg)");
      }
      wasm_function = C->print_i64_func();
    } else {
      FATAL1("Codegen could not find function to call: %s",
             function.ToCString());
    }
  }
  ASSERT(instr->type_args_len() == 0);
  for (intptr_t i = 0; i < num_params; ++i) {
    PushValue(instr->ArgumentValueAt(i)->definition());
  }
  wasm_scope_->AddCall(wasm_function);
  // Note that checking the return value of wasm_function is not an option here
  // since wasm_function might still have the dummy signature if it hasn't been
  // compiled yet.
  if (!AbstractType::Handle(function.result_type()).IsVoidType()) {
    PopValue(instr);
  }
}

void WasmTranslator::VisitReturn(ReturnInstr* instr) {
  // Push return value to Wasm operand stack, unless
  // the current function returns void.
  if (!AbstractType::Handle(function_.result_type()).IsVoidType()) {
    PushValue(instr->value()->definition());
  }
  wasm_scope_->AddReturn();
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

wasm::ValueType* WasmTranslator::GetWasmType(Definition* def) {
  // Get definition type.
  const Class& klass =
      Class::Handle(def->Type()->ToAbstractType()->type_class());
  return C->GetWasmType(klass);
}

wasm::Local* WasmTranslator::GetWasmLocal(Definition* def) {
  wasm::Local* wasm_local = def_to_wasm_local_.LookupValue(def);
  if (wasm_local == nullptr) {
    wasm_local =
        wasm_function_->AddLocal(wasm::Local::Kind::kLocal, GetWasmType(def),
                                 OS::SCreate(codegen_->module_builder()->zone(),
                                             "v%" Pd, def->ssa_temp_index()));
    def_to_wasm_local_.Insert({def, wasm_local});
  }
  return wasm_local;
}

void WasmTranslator::PushValue(Definition* def) {
  // Optimization: for constant definitions, just push the
  // corresponding constant straight away.
  if (auto const_instr = def->AsConstant()) {
    // IL instruction is t <- Constant(...).
    const Object& value = const_instr->value();
    // Hmm, let's see what we do for nulls.
    /*if (value.IsNull()) {
      // t <- Constant(null)
      wasm_scope_->AddRefNull(M->MakeHeapType(GetWasmType(def)));  // ref.null
    }*/
    if (value.IsInteger()) {
      // Treats both Constant and UnboxedConstant instructions.
      int64_t int_value = Integer::GetInt64Value(Integer::Cast(value).raw());
      wasm_scope_->AddI64Constant(int_value);
    } else {
      FATAL("Only integer constants are supported.");
    }
    // Constants of type objects?
  } else if (auto box_integer = def->AsBoxInt64()) {
    // A nop.
    PushValue(box_integer->value()->definition());
  } else if (auto unbox_integer = def->AsUnboxInt64()) {
    // A nop.
    PushValue(unbox_integer->value()->definition());
  } else if (auto int_conv = def->AsIntConverter()) {
    // A nop.
    PushValue(int_conv->value()->definition());
  } else if (auto param = def->AsParameter()) {
    const intptr_t param_index = param->index();
    if (param_index == 0 && function_.HasThisParameter()) {
      // TODO(andreicostin): Implement logic for the "This" parameter.
      UNIMPLEMENTED();
    } else {
      wasm_scope_->AddLocalGet(wasm_function_->GetLocalByIndex(param_index));
    }
  } else {
    // For box integer just use the same one possibly.
    wasm_scope_->AddLocalGet(GetWasmLocal(def));
  }
}

// Emit Wasm local.get instruction to pop value from the Wasm operand stack
// into the Wasm local corresponding to a definition.
void WasmTranslator::PopValue(Definition* def) {
  if (!def->HasSSATemp()) {
    FATAL1("Only definitions with an SSA temp can be popped %s",
           def->ToCString());
  }
  wasm_scope_->AddLocalSet(GetWasmLocal(def));
}

void WasmTranslator::EmitWasmBranch(intptr_t label) {
  // Note that in Wasm labels are uint32_t, but this should not matter
  // for small programs.
  wasm_scope_->AddBr(label);
}

void WasmTranslator::EmitWasmBranchIf(intptr_t label) {
  // Note that in Wasm labels are uint32_t, but this should not matter
  // for small programs.
  wasm_scope_->AddBrIf(label);
}

void WasmTranslator::PushEvalCondition(ComparisonInstr* comp, bool negated) {
  if (comp->InputCount() != 2) {
    FATAL("Unexpected IL input count for comparison.");
  }
  // The three integer cases.
  if (comp->IsRelationalOp() || comp->IsEqualityCompare() ||
      (comp->IsStrictCompare() &&
       IsIntegerValue(
           comp->left()))) {  // RelationalOp >= <= < >, EqualityCompare == !=,
                              // Integer StrictCompare ===, !==
    wasm::IntOp::OpKind op_kind;
    switch (comp->kind()) {
      case Token::Kind::kLT:
        op_kind = wasm::IntOp::OpKind::kLt;
        break;
      case Token::Kind::kGT:
        op_kind = wasm::IntOp::OpKind::kGt;
        break;
      case Token::Kind::kLTE:
        op_kind = wasm::IntOp::OpKind::kLe;
        break;
      case Token::Kind::kGTE:
        op_kind = wasm::IntOp::OpKind::kGe;
        break;
      case Token::Kind::kEQ:
      case Token::Kind::kEQ_STRICT:
        op_kind = wasm::IntOp::OpKind::kEq;
        break;
      case Token::Kind::kNE:
      case Token::Kind::kNE_STRICT:
        op_kind = wasm::IntOp::OpKind::kNeq;
        break;
      default:
        FATAL(
            "Relational/Equality/Strict Integer comparison operator with "
            "unexpected token "
            "kind.");
    }
    if (negated) {
      op_kind = wasm::IntOp::NegateOpKind(op_kind);
    }
    PushValue(comp->left()->definition());
    PushValue(comp->right()->definition());
    wasm_scope_->AddIntOp(wasm::IntOp::IntegerKind::kI64, op_kind);
  } else if (auto strict_comp_op =
                 comp->AsStrictCompare()) {  // General StrictCompare === !==
    // TODO(askesc): Figure out the details.
    UNIMPLEMENTED();
  }
}

void WasmTranslator::StartWasmBlock(BlockEntryInstr* target) {
  // Start Wasm block with output types matching the phi nodes of target.
  scope_stack_.Add(target);
  wasm::FuncType* block_type = M->MakeFuncType();
  if (auto join = target->AsJoinEntry()) {
    if (join->phis() != nullptr) {
      for (intptr_t i = 0; i < join->phis()->length(); i++) {
        block_type->AddResult(GetWasmType(join->phis()->At(i)));
      }
    }
  }
  enclosing_wasm_scopes_stack_.Add(wasm_scope_);
  wasm_scope_ = wasm_scope_->AddBlock(block_type)->body();
}

void WasmTranslator::EndWasmScope() {
  if (enclosing_wasm_scopes_stack_.is_empty()) {
    wasm_scope_ = nullptr;
  } else {
    wasm_scope_ = enclosing_wasm_scopes_stack_.RemoveLast();
  }
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
  // End current Wasm block.
  EndWasmScope();
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
  // Start Wasm loop with input types matching the phi nodes of target.
  scope_stack_.Add(target);
  wasm::FuncType* block_type = M->MakeFuncType();
  if (target->phis() != nullptr) {
    for (intptr_t i = 0; i < target->phis()->length(); i++) {
      block_type->AddParam(GetWasmType(target->phis()->At(i)));
    }
  }
  enclosing_wasm_scopes_stack_.Add(wasm_scope_);
  wasm_scope_ = wasm_scope_->AddLoop(block_type)->body();
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
  // End current Wasm loop.
  EndWasmScope();
}

}  // namespace dart