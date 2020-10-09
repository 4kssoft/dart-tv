// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef RUNTIME_VM_COMPILER_AOT_WASM_TRANSLATOR_H_
#define RUNTIME_VM_COMPILER_AOT_WASM_TRANSLATOR_H_

#include "vm/compiler/aot/wasm_codegen.h"
#include "vm/compiler/backend/flow_graph.h"
#include "vm/compiler/backend/il.h"
#include "vm/growable_array.h"

namespace dart {

class WasmTranslator : public FlowGraphVisitor {
 public:
  explicit WasmTranslator(FlowGraph* flow_graph,
                          WasmCodegen* codegen,
                          const Function& function,
                          wasm::Function* wasm_function)
      : FlowGraphVisitor(flow_graph->reverse_postorder()),
        flow_graph_(flow_graph),
        codegen_(codegen),
        wasm_scope_(wasm_function->MakeNewBodyAndClearLocals()),
        function_(Function::ZoneHandle(function.raw())),
        wasm_function_(wasm_function),
        irreducible_(false) {
    // Add param locals.
    intptr_t num_params = wasm_function_->type()->NumParams();
    for (intptr_t i = 0; i < num_params; ++i) {
      wasm::ValueType* const param_type =
          wasm_function_->type()->ParamTypeAt(i);
      wasm_function_->AddLocal(
          wasm::Local::Kind::kParam, param_type,
          String::ZoneHandle(codegen->module_builder()->zone(),
                             function.ParameterNameAt(i))
              .ToCString());
    }
  }

  void Translate() {
    PrepareBlocks();
    VisitBlocks();
  }

  bool irreducible() { return irreducible_; }

  virtual void VisitFunctionEntry(FunctionEntryInstr* block);
  virtual void VisitJoinEntry(JoinEntryInstr* block);
  virtual void VisitTargetEntry(TargetEntryInstr* block);
  virtual void VisitGoto(GotoInstr* instr);
  virtual void VisitBranch(BranchInstr* instr);
  virtual void VisitBinaryIntegerOp(BinaryIntegerOpInstr* instr);
  virtual void VisitBinaryInt32Op(BinaryInt32OpInstr* instr);
  virtual void VisitBinaryInt64Op(BinaryInt64OpInstr* instr);
  virtual void VisitBinaryUint32Op(BinaryUint32OpInstr* instr);
  virtual void VisitBooleanNegate(BooleanNegateInstr* instr);
  virtual void VisitParameter(ParameterInstr* instr);
  virtual void VisitStaticCall(StaticCallInstr* instr);
  virtual void VisitDispatchTableCall(DispatchTableCallInstr* instr);
  virtual void VisitReturn(ReturnInstr* instr);
  virtual void VisitAllocateObject(AllocateObjectInstr* instr);
  virtual void VisitLoadField(LoadFieldInstr* instr);
  virtual void VisitStoreInstanceField(StoreInstanceFieldInstr* instr);
  virtual void VisitEqualityCompare(EqualityCompareInstr* instr);
  virtual void VisitStrictCompare(StrictCompareInstr* instr);
  virtual void VisitRelationalOp(RelationalOpInstr* instr);

 private:
  // Traverse graph to find start locations of Wasm blocks.
  void PrepareBlocks();

  intptr_t GetFallthroughPredecessorIndex(BlockEntryInstr* target);
  intptr_t GetSourcePredecessorIndex(BlockEntryInstr* target,
                                     BlockEntryInstr* source);
  intptr_t GetLabelForTarget(BlockEntryInstr* target);

  void PushPhiValues(JoinEntryInstr* join, intptr_t pred_index);
  void PopPhiValues(JoinEntryInstr* join);

  // The type hint helps in cases where it can not be determined otherwise
  // (i.e. for pushing an appropriately-typed Wasm null value). Can be nullptr
  // if the caller can guarantee that it is not needed.
  void PushValue(Definition* def, const AbstractType& type_hint);
  void PopValue(Definition* def);
  void EmitWasmBranch(intptr_t label);
  void EmitWasmBranchIf(intptr_t label);

  // Emit code for computing the result of a comparison IL
  // instruction, and negate it if negate is true. Push the
  // resulting condition on the stack as an i32.
  void PushEvalCondition(ComparisonInstr* comp, bool negated);

  void StartWasmBlock(BlockEntryInstr* target);
  void EndWasmBlock(bool pop_phi_values);
  void StartWasmLoop(bool push_phi_values);
  void EndWasmLoop(BlockEntryInstr* source);
  void EndWasmScope();

  wasm::ValueType* GetWasmType(Definition* def);
  wasm::Local* GetWasmLocal(Definition* def);

  static Class& GetTypeClass(const AbstractType& type) {
    return Class::Handle(type.type_class());
  }
  static Class& GetTypeClass(CompileType* type) {
    return GetTypeClass(*type->ToAbstractType());
  }
  static Class& GetTypeClass(Value* value) {
    return GetTypeClass(value->Type());
  }
  static bool IsIntegerValue(Value* value) {
    return WasmCodegen::IsIntegerClass(GetTypeClass(value));
  }
  static bool IsBoolValue(Value* value) {
    return WasmCodegen::IsBoolClass(GetTypeClass(value));
  }

  FlowGraph* const flow_graph_;
  WasmCodegen* const codegen_;

  // The following stacks are used in our implementation of the Stackifier
  // Algorithm for recovering structured control flow from arbitrary CFGs:
  // - Target blocks of forward and backward edges, sorted (reversed) by the
  // order in which the corresponding Wasm block or loop should be started.
  GrowableArray<BlockEntryInstr*> edge_stack_;
  // - For each backward edge on the edge stack, how many Wasm blocks should be
  // started right inside the loop block (because their corresponding targets
  // reside inside the loop).
  GrowableArray<intptr_t> forward_count_stack_;
  // - The target blocks of the current stack of Wasm scopes. The label index
  // for a forward jump can be found by searching from the top for the target.
  GrowableArray<BlockEntryInstr*> scope_stack_;

  // The stack of currently open Wasm scopes (blocks, loops and ifs), other
  // than the current one below. This is used to reset the current scope
  // pointer when it is closed.
  GrowableArray<wasm::InstructionList*> enclosing_wasm_scopes_stack_;
  wasm::InstructionList* wasm_scope_;

  // Dart function whose body is being generated.
  const Function& function_;

  // Wasm function whose body is being generated.
  wasm::Function* const wasm_function_;

  // Mapping from definitions of SSA temps to their assigned Wasm locals.
  DirectChainedHashMap<RawPointerKeyValueTrait<Definition, wasm::Local*>>
      def_to_wasm_local_;

  // Did the transformation fail because the graph is not reducible?
  bool irreducible_;
};

}  // namespace dart

#endif  // RUNTIME_VM_COMPILER_AOT_WASM_TRANSLATOR_H_
