library dart2js.cps_ir.update_refinements;

import 'cps_ir_nodes.dart';
import 'optimizers.dart' show Pass;
import 'type_mask_system.dart';

/// Updates all references to use the most refined version in scope.
///
/// [GVN] and [RedundantJoinElimination], and possibly other passes, can create
/// references that don't use the best refinement in scope. This pass improves
/// the refinement information.
///
//
// TODO(asgerf): Could be done during GVN for another adjacent pass.
//   It is easier to measure performance and rearrange passes when it has its
//   own pass, but we can merge it with an adjacent pass later.
//
class UpdateRefinements extends TrampolineRecursiveVisitor implements Pass {
  String get passName => 'Update refinements';

  final TypeMaskSystem typeSystem;

  Map<Primitive, Primitive> refinementFor = <Primitive, Primitive>{};

  UpdateRefinements(this.typeSystem);

  void rewrite(FunctionDefinition node) {
    visit(node);
  }

  Expression traverseLetPrim(LetPrim node) {
    Expression next = node.body;
    visit(node.primitive);
    return next;
  }

  visitNullCheck(NullCheck node) {
    if (refine(node.value)) {
      Primitive value = node.value.definition;
      if (value.type.isNullable) {
        // Update the type if the input has changed.
        node.type = value.type.nonNullable();
      } else {
        node..replaceUsesWith(value)..destroy();
        LetPrim letPrim = node.parent;
        letPrim.remove();
        return;
      }
    }
    // Use the NullCheck as a refinement.
    Primitive value = node.effectiveDefinition;
    Refinement old = refinementFor[value];
    refinementFor[value] = node;
    pushAction(() {
      refinementFor[value] = old;
    });
  }

  visitRefinement(Refinement node) {
    if (refine(node.value)) {
      // Update the type if the input has changed.
      node.type = typeSystem.intersection(node.value.definition.type,
          node.refineType);
    }
    Primitive value = node.effectiveDefinition;
    Refinement old = refinementFor[value];
    refinementFor[value] = node;
    pushAction(() {
      refinementFor[value] = old;
    });
  }

  processReference(Reference ref) {
    refine(ref);
  }

  bool refine(Reference ref) {
    Definition def = ref.definition;
    if (def is Primitive) {
      Refinement refinement = refinementFor[def.effectiveDefinition];
      if (refinement != null && refinement != ref.definition) {
        ref.changeTo(refinement);
        return true;
      }
    }
    return false;
  }
}
