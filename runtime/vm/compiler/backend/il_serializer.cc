// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/compiler/backend/il_serializer.h"

#include "platform/text_buffer.h"
#include "vm/compiler/aot/llvm_serializer.h"
#include "vm/hash_table.h"
#include "vm/log.h"
#include "vm/object_store.h"

namespace dart {

#if !defined(DART_PRECOMPILED_RUNTIME)

namespace {
intptr_t CountInstructions(BlockEntryInstr* instr) {
  intptr_t instr_count = 0;
  ForwardInstructionIterator it(instr);
  auto has_comparison = [&](auto instr) {
    return instr->IsBranch() || instr->IsIfThenElse();
  };
  for (; !it.Done(); it.Advance()) {
    instr_count += 1 + has_comparison(it.Current());
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

ILSerializer::ILSerializer(const FlowGraph& flow_graph,
                           LLVMSerializer* llvm_serializer)
    : FlowGraphVisitor(flow_graph.reverse_postorder()),
      num_blocks_(flow_graph.reverse_postorder().length()),
      flow_graph_(flow_graph),
      llvm_serializer_(llvm_serializer) {}

void ILSerializer::SerializeValue(Value* val) const {
  THR_Print(" v%" Pd, val->definition()->ssa_temp_index());
}

void ILSerializer::VisitInstruction(Instruction* instr) {
  // Prints v-1 for comparison instructions
  if (instr->IsIfThenElse()) {
    VisitInstruction(instr->AsIfThenElse()->comparison());
    THR_Print("\n");
  }
  if (instr->IsDefinition() &&
      (instr->IsComparison() || instr->AsDefinition()->ssa_temp_index() >= 0)) {
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
  const auto& fn = flow_graph_.function();
  SerializeFunctionDeclaration(fn);
  if (fn.HasOptionalParameters()) {
    for (intptr_t i = 0; i < fn.NumOptionalParameters(); ++i) {
      intptr_t default_const_id =
          AddConstant(flow_graph_.parsed_function().DefaultParameterValueAt(i));
      THR_Print("%" Pd "\n", default_const_id);
    }
  }
  THR_Print("%" Pd "\n", num_blocks_);
  VisitBlocks();
}

void ILSerializer::SerializeFunctionDeclaration(const Function& fn) {
  const auto& name = String::Handle(fn.name());
  THR_Print("%s\n", name.ToCString());
  THR_Print("%" Pd32 "\n", fn.IsStaticFunction());
  THR_Print("%" Pd "\n", fn.NumParameters());
  THR_Print("%" Pd "\n", fn.NumOptionalPositionalParameters());
  THR_Print("%" Pd "\n", fn.NumOptionalNamedParameters());
  for (intptr_t i = 0; i < fn.NumOptionalNamedParameters(); ++i) {
    auto param_idx = i + fn.num_fixed_parameters();
    auto& param_name = String::Handle(fn.ParameterNameAt(param_idx));
    THR_Print("%s\n", param_name.ToCString());
  }
}

void ILSerializer::PrintSerialization(FlowGraph* flow_graph,
                                      LLVMSerializer* llvm_serializer) {
  llvm_serializer->AddFunction(flow_graph->function());
  ILSerializer serializer(*flow_graph, llvm_serializer);
  serializer.Serialize();
}

void ILSerializer::VisitGraphEntry(GraphEntryInstr* instr) {
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

  current_join_ = instr;
  if (instr->phis() != nullptr) {
    for (PhiInstr* phi : *instr->phis()) {
      VisitInstruction(phi);
      THR_Print("\n");
    }
  }
  current_join_ = nullptr;
}

void ILSerializer::VisitTargetEntry(TargetEntryInstr* instr) {
  THR_Print("%" Pd "\n", instr->block_id());
}

void ILSerializer::VisitFunctionEntry(FunctionEntryInstr* instr) {
  THR_Print("%" Pd "\n", instr->block_id());

  if (instr->initial_definitions() != nullptr) {
    for (Definition* def : *instr->initial_definitions()) {
      VisitInstruction(def);
      THR_Print("\n");
    }
  }
}

void ILSerializer::VisitConstant(ConstantInstr* instr) {
  THR_Print("%s", instr->DebugName());
  THR_Print(" %" Pd, AddConstant(instr->value()));
}

void ILSerializer::VisitParameter(ParameterInstr* instr) {
  THR_Print("%s", instr->DebugName());
  THR_Print(" %" Pd, instr->index());
}

void ILSerializer::VisitUnboxInt64(UnboxInt64Instr* instr) {
  THR_Print("%s", instr->DebugName());
  auto val = instr->value();
  THR_Print(" %" Pd32, val->Type()->ToCid() == kSmiCid);
  SerializeValue(val);
}

void ILSerializer::VisitUnboxedConstant(UnboxedConstantInstr* instr) {
  // TODO(sarkin): Other representations
  THR_Print("%s", instr->DebugName());
  auto& value = instr->value();
  if (instr->representation() == kUnboxedInt32 ||
      instr->representation() == kUnboxedInt64) {
    const int64_t val =
        value.IsSmi() ? Smi::Cast(value).Value() : Mint::Cast(value).value();
    THR_Print(" %" Pd32 " %" Pd64, instr->representation(), val);
  } else {
    ASSERT(false);
  }
}

void ILSerializer::VisitPhi(PhiInstr* instr) {
  assert(current_join_ != nullptr);
  assert(current_join_->PredecessorCount() == instr->InputCount());
  THR_Print("%s", instr->DebugName());

  THR_Print(" %" Pd32, instr->representation());
  for (intptr_t i = 0; i < instr->InputCount(); i++) {
    auto input = instr->InputAt(i)->definition();
    THR_Print(" v%" Pd " %" Pd, input->ssa_temp_index(),
              current_join_->PredecessorAt(i)->block_id());
  }
}

void ILSerializer::VisitCheckStackOverflow(CheckStackOverflowInstr* instr) {
  THR_Print("%s", instr->DebugName());
}

void ILSerializer::VisitCheckNull(CheckNullInstr* instr) {
  THR_Print("%s", instr->DebugName());
  SerializeValue(instr->value());
}

void ILSerializer::VisitBranch(BranchInstr* instr) {
  VisitInstruction(instr->comparison());
  THR_Print("\n");

  THR_Print("%s", instr->DebugName());
  THR_Print(" %" Pd " %" Pd, instr->true_successor()->block_id(),
            instr->false_successor()->block_id());
}

void ILSerializer::SerializeIndexOrConstant(Definition* d) const {
  THR_Print(" ");
  if (d->IsConstant()) {
    if (d->AsConstant()->IsUnboxedSignedIntegerConstant()) {
      THR_Print("%" Pd,
                d->AsConstant()->GetUnboxedSignedIntegerConstantValue());
    } else {
      // Smi case
      THR_Print("%" Pd,
                reinterpret_cast<int64_t>(d->AsConstant()->value().raw()));
    }
  } else {
    THR_Print("v%" Pd, d->ssa_temp_index());
  }
}

template <typename T>
void ILSerializer::SerializeCompare(T instr) const {
  intptr_t operation_cid = instr->operation_cid();
  ASSERT(operation_cid == kSmiCid || operation_cid == kMintCid ||
         operation_cid == kDoubleCid);
  dart_llvm::RelationalOpCid op_cid = (operation_cid == kDoubleCid)
                                          ? dart_llvm::RelationalOpCid::kDouble
                                          : dart_llvm::RelationalOpCid::kInt64;
  THR_Print("%s", instr->DebugName());
  THR_Print(" %" Pd32 " %" Pd32, instr->kind(), op_cid);

  SerializeIndexOrConstant(instr->left()->definition());
  SerializeIndexOrConstant(instr->right()->definition());
}

void ILSerializer::VisitRelationalOp(RelationalOpInstr* instr) {
  SerializeCompare(instr);
}

void ILSerializer::VisitEqualityCompare(EqualityCompareInstr* instr) {
  SerializeCompare(instr);
}

void ILSerializer::VisitStrictCompare(StrictCompareInstr* instr) {
  THR_Print("%s", instr->DebugName());
  THR_Print(" %" Pd32 " %" Pd32, instr->kind(), instr->needs_number_check());
  SerializeValue(instr->left());
  SerializeValue(instr->right());
}

void ILSerializer::VisitBinaryInt64Op(BinaryInt64OpInstr* instr) {
  THR_Print("%s", instr->DebugName());
  THR_Print(" %" Pd32, instr->op_kind());

  SerializeIndexOrConstant(instr->left()->definition());
  SerializeIndexOrConstant(instr->right()->definition());
}

void ILSerializer::VisitBoxInt64(BoxInt64Instr* instr) {
  THR_Print("%s", instr->DebugName());
  THR_Print(" %" Pd32, instr->ValueFitsSmi());
  SerializeValue(instr->value());
}

void ILSerializer::VisitGoto(GotoInstr* instr) {
  THR_Print("%s", instr->DebugName());
  THR_Print(" %" Pd, instr->successor()->block_id());
}

void ILSerializer::VisitReturn(ReturnInstr* instr) {
  THR_Print("%s", instr->DebugName());
  SerializeValue(instr->value());
}

void ILSerializer::VisitPushArgument(PushArgumentInstr* instr) {
  THR_Print("%s", instr->DebugName());
}

void ILSerializer::VisitLoadIndexedUnsafe(LoadIndexedUnsafeInstr* instr) {
  THR_Print("%s", instr->DebugName());
  ASSERT(instr->offset() % kWordSize == 0);
  // last parameter <- rbp[0 + 2 * kWordSize]
  // ...
  // first parameter <- rbp[0 + (num_params + 1) * kWordSize]
  intptr_t loaded_parameter_num = flow_graph_.function().NumParameters() -
                                  (instr->offset() / kWordSize - 1);
  THR_Print(" %" Pd, loaded_parameter_num);
}

void ILSerializer::SerializeArgumentsDescriptor(
    const ArgumentsDescriptor& arg_descriptor) const {
  THR_Print(" %" Pd, arg_descriptor.NamedCount());
  for (intptr_t i = 0; i < arg_descriptor.NamedCount(); ++i) {
    auto& named_i = String::Handle(arg_descriptor.NameAt(i));
    THR_Print(" %s", named_i.ToCString());
  }
}

void ILSerializer::SerializeCall(const TemplateDartCall<0>* instr,
                                 bool serialize_arguments) {
  ArgumentsDescriptor arg_descriptor(
      Array::Handle(instr->GetArgumentsDescriptor()));
  THR_Print(" %" Pd,
            AddConstant(Object::Handle(instr->GetArgumentsDescriptor())));
  SerializeArgumentsDescriptor(arg_descriptor);
  if (serialize_arguments) {
    for (intptr_t i = instr->FirstArgIndex(); i < instr->ArgumentCount(); ++i) {
      SerializeValue(instr->PushArgumentAt(i)->value());
    }
  }
}

void ILSerializer::VisitStaticCall(StaticCallInstr* instr) {
  // TODO(sarkin):
  THR_Print("%s", instr->DebugName());
  bool is_llvm = instr->function().IsLLVMCompiled();
  THR_Print(" %" Pd32, is_llvm);

  if (!is_llvm) {
    instr->function().set_dart_id(AddDartFunction(instr->function()));
  }

  char patch_point[100];
  BufferFormatter formatter(patch_point, 100);
  formatter.Print("%" Pd32 "#%" Pd
                  "#"
                  "%" Pd,
                  is_llvm, flow_graph_.function().llvm_id(),
                  static_call_count_++);
  llvm_serializer_->static_call_patch_map_.InsertOrGetValue(
      String::Handle(String::New(patch_point)), instr->function());
  THR_Print(" %s", patch_point);

  SerializeCall(instr);
}

void ILSerializer::SerializeInstanceCall(InstanceCallInstr* instr,
                                         bool check_smi,
                                         bool serialize_arguments) {
  THR_Print(" %s %" Pd32 " ", instr->function_name().ToCString(), check_smi);
  SerializeCall(instr, serialize_arguments);
}

void ILSerializer::VisitInstanceCall(InstanceCallInstr* instr) {
  THR_Print("%s ", instr->DebugName());
  const AbstractType& value_type = *instr->Receiver()->Type()->ToAbstractType();
  bool check_smi = (CompileType::Smi().IsAssignableTo(value_type) ||
                    value_type.IsTypeParameter());
  SerializeInstanceCall(instr, check_smi);
}

void ILSerializer::VisitPolymorphicInstanceCall(
    PolymorphicInstanceCallInstr* instr) {
  THR_Print("%s ", instr->DebugName());
  // TODO(sarkin): Check smi?
  SerializeInstanceCall(instr->instance_call(), true);
}

void ILSerializer::VisitAssertBoolean(AssertBooleanInstr* instr) {
  THR_Print("%s", instr->DebugName());
  SerializeValue(instr->value());
}

void ILSerializer::VisitLoadClassId(LoadClassIdInstr* instr) {
  THR_Print("%s", instr->DebugName());
  const AbstractType& value_type = *instr->object()->Type()->ToAbstractType();
  bool check_smi = (CompileType::Smi().IsAssignableTo(value_type) ||
                    value_type.IsTypeParameter());
  THR_Print(" %" Pd32, check_smi);
  SerializeValue(instr->object());
}

void ILSerializer::VisitLoadStaticField(LoadStaticFieldInstr* instr) {
  THR_Print("%s", instr->DebugName());
  SerializeValue(instr->field_value());
}

void ILSerializer::VisitStoreStaticField(StoreStaticFieldInstr* instr) {
  THR_Print("%s", instr->DebugName());

  THR_Print(" %" Pd32, instr->value()->NeedsWriteBarrier());
  const intptr_t cid = instr->value()->Type()->ToNullableCid();
  THR_Print(" %" Pd32, cid != kSmiCid && instr->CanValueBeSmi());

  auto& field = Field::Handle(instr->field().Original());
  THR_Print(" %" Pd, AddConstant(field));
  SerializeValue(instr->value());
}

void ILSerializer::VisitCheckedSmiComparison(CheckedSmiComparisonInstr* instr) {
  THR_Print("%s", instr->DebugName());
  String& selector = String::Handle(instr->call()->ic_data()->target_name());
  THR_Print(" %" Pd32, instr->kind());
  THR_Print(" %s", selector.ToCString());
  THR_Print(" %" Pd32, instr->is_negated());

  THR_Print(" %" Pd32, instr->left()->Type()->ToCid() == kSmiCid);
  THR_Print(" %" Pd32, instr->right()->Type()->ToCid() == kSmiCid);

  SerializeValue(instr->left());
  SerializeValue(instr->right());

  SerializeInstanceCall(instr->call(), /* check_smi */ false, false);

  SerializeValue(instr->left());
  SerializeValue(instr->right());
}

void ILSerializer::VisitAssertAssignable(AssertAssignableInstr* instr) {
  // TODO(sarkin):
  THR_Print("%s", instr->DebugName());
}

void ILSerializer::VisitAllocateObject(AllocateObjectInstr* instr) {
  THR_Print("%s", instr->DebugName());
  THR_Print(" %" Pd " %" Pd, instr->cls().id(), AddConstant(instr->cls()));
  for (intptr_t i = 0; i < instr->ArgumentCount(); ++i) {
    SerializeValue(instr->PushArgumentAt(i)->value());
  }
}

void ILSerializer::VisitCreateArray(CreateArrayInstr* instr) {
  THR_Print("%s", instr->DebugName());
  SerializeValue(instr->num_elements());
  SerializeValue(instr->element_type());
}

void ILSerializer::VisitLoadField(LoadFieldInstr* instr) {
  THR_Print("%s", instr->DebugName());
  THR_Print(" %" Pd32 " %" Pd32, instr->IsUnboxedLoad(),
            instr->IsPotentialUnboxedLoad());
  THR_Print(" %" Pd32, instr->representation());
  THR_Print(" %" Pd, instr->offset_in_bytes());
  SerializeValue(instr->instance());
  if (instr->field() != nullptr) {
    auto& field = Field::Handle(instr->field()->Original());
    THR_Print(" %" Pd, AddConstant(field));
  } else {
    THR_Print(" 0");
  }
}

void ILSerializer::VisitStoreInstanceField(StoreInstanceFieldInstr* instr) {
  THR_Print("%s", instr->DebugName());
  THR_Print(" %" Pd32 " %" Pd32, instr->IsUnboxedStore(),
            instr->is_initialization());
  THR_Print(" %" Pd32, instr->IsPotentialUnboxedStore());
  THR_Print(" %" Pd32, instr->ShouldEmitStoreBarrier());
  SerializeValue(instr->instance());
  SerializeValue(instr->value());
  if (!instr->field().IsNull()) {
    THR_Print(" %" Pd, instr->field().UnboxedFieldCid());
  } else {
    THR_Print(" 0");
  }
  THR_Print(" %" Pd, instr->offset_in_bytes());
  const intptr_t cid = instr->value()->Type()->ToNullableCid();
  THR_Print(" %" Pd32, cid != kSmiCid && instr->CanValueBeSmi());
  auto& field = Field::Handle(instr->field().Original());
  THR_Print(" %" Pd, AddConstant(field));
}

void ILSerializer::VisitSpecialParameter(SpecialParameterInstr* instr) {
  // TODO(sarkin): Serialize the type.
  THR_Print("%s", instr->DebugName());
}

static bool CanBeImmediateIndex(Value* index, intptr_t cid) {
  if (!index->definition()->IsConstant()) return false;
  const Object& constant = index->definition()->AsConstant()->value();
  if (!constant.IsSmi()) return false;
  const Smi& smi_const = Smi::Cast(constant);
  const intptr_t scale = Instance::ElementSizeFor(cid);
  const intptr_t data_offset = Instance::DataOffsetFor(cid);
  const int64_t disp = smi_const.AsInt64Value() * scale + data_offset;
  return Utils::IsInt(32, disp);
}

void ILSerializer::VisitLoadIndexed(LoadIndexedInstr* instr) {
  THR_Print("%s", instr->DebugName());
  THR_Print(" %" Pd32, instr->IsExternal());
  THR_Print(" %" Pd32, instr->representation());
  THR_Print(" %" Pd, instr->index_scale());
  THR_Print(" %" Pd, instr->class_id());
  if (!instr->IsExternal()) {
    THR_Print(" %" Pd, Instance::DataOffsetFor(instr->class_id()));
  }
  THR_Print(" v%" Pd, instr->array()->definition()->ssa_temp_index());
  auto index = instr->index()->definition();
  if (CanBeImmediateIndex(instr->index(), instr->class_id())) {
    THR_Print(" %" Pd, Smi::Cast(index->AsConstant()->value()).Value());
  } else {
    THR_Print(" v%" Pd, index->ssa_temp_index());
  }
}

void ILSerializer::VisitStoreIndexed(StoreIndexedInstr* instr) {
  THR_Print("%s", instr->DebugName());
  THR_Print(" %" Pd32, instr->ShouldEmitStoreBarrier());
  THR_Print(" %" Pd32, instr->IsExternal());
  THR_Print(" %" Pd, instr->index_scale());
  THR_Print(" %" Pd, instr->class_id());
  if (!instr->IsExternal()) {
    THR_Print(" %" Pd, Instance::DataOffsetFor(instr->class_id()));
  }
  const intptr_t cid = instr->value()->Type()->ToNullableCid();
  THR_Print(" %" Pd32, cid != kSmiCid && instr->CanValueBeSmi());
  THR_Print(" v%" Pd, instr->array()->definition()->ssa_temp_index());
  auto index = instr->index()->definition();
  if (CanBeImmediateIndex(instr->index(), instr->class_id())) {
    THR_Print(" %" Pd, Smi::Cast(index->AsConstant()->value()).Value());
  } else {
    THR_Print(" v%" Pd, index->ssa_temp_index());
  }
  auto value = instr->value()->definition();
  THR_Print(" v%" Pd, value->ssa_temp_index());
  bool smi = false;
  if (value->IsConstant()) {
    auto& raw_value = value->AsConstant()->value();
    if (raw_value.IsSmi()) {
      smi = true;
      THR_Print(" 1 %" Pd, Smi::Cast(raw_value).Value());
    }
  }
  if (!smi) {
    THR_Print(" 0");
  }
}

void ILSerializer::VisitOneByteStringFromCharCode(
    OneByteStringFromCharCodeInstr* instr) {
  THR_Print("%s", instr->DebugName());
  THR_Print(" v%" Pd, instr->char_code()->definition()->ssa_temp_index());
}

void ILSerializer::VisitGenericCheckBound(GenericCheckBoundInstr* instr) {
  // TODO(sarkin):
  THR_Print("%s", instr->DebugName());
}

void ILSerializer::VisitBooleanNegate(BooleanNegateInstr* instr) {
  THR_Print("%s", instr->DebugName());
  THR_Print(" v%" Pd, instr->value()->definition()->ssa_temp_index());
}

void ILSerializer::VisitIfThenElse(IfThenElseInstr* instr) {
  THR_Print("%s", instr->DebugName());
  THR_Print(" %" Pd, instr->if_true());
  THR_Print(" %" Pd, instr->if_false());
}

#endif  // !DART_PRECOMPILED_RUNTIME

}  // namespace dart
