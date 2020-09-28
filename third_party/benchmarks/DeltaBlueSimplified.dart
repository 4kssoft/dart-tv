// Copyright 2011 Google Inc. All Rights Reserved.
// Copyright 1996 John Maloney and Mario Wolczko
//
// This file is part of GNU Smalltalk.
//
// GNU Smalltalk is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 2, or (at your option) any later version.
//
// GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
// Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
//
// Translated first from Smalltalk to JavaScript, and finally to
// Dart by Google 2008-2010.

// This version is further simplified to only use language constructs supported
// by the Dart-to-Wasm prototype in 2020.

/**
 * A Dart implementation of the DeltaBlue constraint-solving
 * algorithm, as described in:
 *
 * "The DeltaBlue Algorithm: An Incremental Constraint Hierarchy Solver"
 *   Bjorn N. Freeman-Benson and John Maloney
 *   January 1990 Communications of the ACM,
 *   also available as University of Washington TR 89-08-06.
 *
 * Beware: this benchmark is written in a grotesque style where
 * the constraint model is built by side-effects from constructors.
 * I've kept it this way to avoid deviating too much from the original
 * implementation.
 */

main() {
  new DeltaBlue().run();
}

/// Benchmark class required to report results.
class DeltaBlue {
  void run() {
    chainTest(100000);
    projectionTest(100000);
  }
}

/**
 * Strengths are used to measure the relative importance of constraints.
 * New strengths may be inserted in the strength hierarchy without
 * disrupting current constraints.  Strengths cannot be created outside
 * this class, so == can be used for value comparison.
 */
class Strength {
  static int nextWeaker(int s) => s + 1;

  static bool stronger(int s1, int s2) {
    return s1 < s2;
  }

  static bool weaker(int s1, int s2) {
    return s1 > s2;
  }

  static int weakest(int s1, int s2) {
    return weaker(s1, s2) ? s1 : s2;
  }

  static int strongest(int s1, int s2) {
    return stronger(s1, s2) ? s1 : s2;
  }
}

// Compile time computed constants.
const REQUIRED = 0;
const STRONG_PREFERRED = 1;
const PREFERRED = 2;
const STRONG_DEFAULT = 3;
const NORMAL = 4;
const WEAK_DEFAULT = 5;
const WEAKEST = 6;

abstract class Constraint {
  final Planner planner;
  final int strength;

  Constraint(this.planner, this.strength);

  bool isSatisfied();
  void markUnsatisfied();
  void addToGraph();
  void removeFromGraph();
  void chooseMethod(int mark);
  void markInputs(int mark);
  bool inputsKnown(int mark);
  Variable output();
  void execute();
  void recalculate();

  /// Activate this constraint and attempt to satisfy it.
  void addConstraint() {
    addToGraph();
    planner.incrementalAdd(this);
  }

  /**
   * Attempt to find a way to enforce this constraint. If successful,
   * record the solution, perhaps modifying the current dataflow
   * graph. Answer the constraint that this constraint overrides, if
   * there is one, or nil, if there isn't.
   * Assume: I am not already satisfied.
   */
  Constraint satisfy(mark) {
    chooseMethod(mark);
    if (!isSatisfied()) {
      if (strength == REQUIRED) {
        errorRequiredConstraint();
      }
      return null;
    }
    markInputs(mark);
    Variable out = output();
    Constraint overridden = out.determinedBy;
    if (overridden != null) overridden.markUnsatisfied();
    out.determinedBy = this;
    if (!planner.addPropagate(this, mark)) errorCycleEncountered();
    out.mark = mark;
    return overridden;
  }

  void destroyConstraint() {
    if (isSatisfied()) planner.incrementalRemove(this);
    removeFromGraph();
  }

  /**
   * Normal constraints are not input constraints.  An input constraint
   * is one that depends on external state, such as the mouse, the
   * keyboard, a clock, or some arbitrary piece of imperative code.
   */
  bool isInput() => false;
}

/**
 * Abstract superclass for constraints having a single possible output variable.
 */
abstract class UnaryConstraint extends Constraint {
  final Variable myOutput;
  bool satisfied = false;

  UnaryConstraint(Planner planner, this.myOutput, int strength)
      : super(planner, strength) {
    addConstraint();
  }

  /// Adds this constraint to the constraint graph
  void addToGraph() {
    myOutput.addConstraint(this);
    satisfied = false;
  }

  /// Decides if this constraint can be satisfied and records that decision.
  void chooseMethod(int mark) {
    satisfied = (myOutput.mark != mark) &&
        Strength.stronger(strength, myOutput.walkStrength);
  }

  /// Returns true if this constraint is satisfied in the current solution.
  bool isSatisfied() => satisfied;

  void markInputs(int mark) {
    // has no inputs.
  }

  /// Returns the current output variable.
  Variable output() => myOutput;

  /**
   * Calculate the walkabout strength, the stay flag, and, if it is
   * 'stay', the value for the current output of this constraint. Assume
   * this constraint is satisfied.
   */
  void recalculate() {
    myOutput.walkStrength = strength;
    myOutput.stay = !isInput();
    if (myOutput.stay) execute(); // Stay optimization.
  }

  /// Records that this constraint is unsatisfied.
  void markUnsatisfied() {
    satisfied = false;
  }

  bool inputsKnown(int mark) => true;

  void removeFromGraph() {
    if (myOutput != null) myOutput.removeConstraint(this);
    satisfied = false;
  }
}

/**
 * Variables that should, with some level of preference, stay the same.
 * Planners may exploit the fact that instances, if satisfied, will not
 * change their output during plan execution.  This is called "stay
 * optimization".
 */
class StayConstraint extends UnaryConstraint {
  StayConstraint(Planner planner, Variable v, int str) : super(planner, v, str);

  void execute() {
    // Stay constraints do nothing.
  }
}

/**
 * A unary input constraint used to mark a variable that the client
 * wishes to change.
 */
class EditConstraint extends UnaryConstraint {
  EditConstraint(Planner planner, Variable v, int str) : super(planner, v, str);

  /// Edits indicate that a variable is to be changed by imperative code.
  bool isInput() => true;

  void execute() {
    // Edit constraints do nothing.
  }
}

// Directions.
const int NONE = 1;
const int FORWARD = 2;
const int BACKWARD = 0;

/**
 * Abstract superclass for constraints having two possible output
 * variables.
 */
abstract class BinaryConstraint extends Constraint {
  Variable v1;
  Variable v2;
  int direction = NONE;

  BinaryConstraint(Planner planner, this.v1, this.v2, int strength)
      : super(planner, strength) {
    addConstraint();
  }

  /**
   * Decides if this constraint can be satisfied and which way it
   * should flow based on the relative strength of the variables related,
   * and record that decision.
   */
  void chooseMethod(int mark) {
    if (v1.mark == mark) {
      direction =
          (v2.mark != mark && Strength.stronger(strength, v2.walkStrength))
              ? FORWARD
              : NONE;
    }
    if (v2.mark == mark) {
      direction =
          (v1.mark != mark && Strength.stronger(strength, v1.walkStrength))
              ? BACKWARD
              : NONE;
    }
    if (Strength.weaker(v1.walkStrength, v2.walkStrength)) {
      direction =
          Strength.stronger(strength, v1.walkStrength) ? BACKWARD : NONE;
    } else {
      direction =
          Strength.stronger(strength, v2.walkStrength) ? FORWARD : BACKWARD;
    }
  }

  /// Add this constraint to the constraint graph.
  void addToGraph() {
    v1.addConstraint(this);
    v2.addConstraint(this);
    direction = NONE;
  }

  /// Answer true if this constraint is satisfied in the current solution.
  bool isSatisfied() => direction != NONE;

  /// Mark the input variable with the given mark.
  void markInputs(int mark) {
    input().mark = mark;
  }

  /// Returns the current input variable
  Variable input() => direction == FORWARD ? v1 : v2;

  /// Returns the current output variable.
  Variable output() => direction == FORWARD ? v2 : v1;

  /**
   * Calculate the walkabout strength, the stay flag, and, if it is
   * 'stay', the value for the current output of this
   * constraint. Assume this constraint is satisfied.
   */
  void recalculate() {
    Variable ihn = input(), out = output();
    out.walkStrength = Strength.weakest(strength, ihn.walkStrength);
    out.stay = ihn.stay;
    if (out.stay) execute();
  }

  /// Record the fact that this constraint is unsatisfied.
  void markUnsatisfied() {
    direction = NONE;
  }

  bool inputsKnown(int mark) {
    Variable i = input();
    return i.mark == mark || i.stay || i.determinedBy == null;
  }

  void removeFromGraph() {
    if (v1 != null) v1.removeConstraint(this);
    if (v2 != null) v2.removeConstraint(this);
    direction = NONE;
  }
}

/**
 * Relates two variables by the linear scaling relationship: "v2 =
 * (v1 * scale) + offset". Either v1 or v2 may be changed to maintain
 * this relationship but the scale factor and offset are considered
 * read-only.
 */

class ScaleConstraint extends BinaryConstraint {
  final Variable scale;
  final Variable offset;

  ScaleConstraint(Planner planner, Variable src, this.scale, this.offset,
      Variable dest, int strength)
      : super(planner, src, dest, strength);

  /// Adds this constraint to the constraint graph.
  void addToGraph() {
    super.addToGraph();
    scale.addConstraint(this);
    offset.addConstraint(this);
  }

  void removeFromGraph() {
    super.removeFromGraph();
    if (scale != null) scale.removeConstraint(this);
    if (offset != null) offset.removeConstraint(this);
  }

  void markInputs(int mark) {
    super.markInputs(mark);
    scale.mark = offset.mark = mark;
  }

  /// Enforce this constraint. Assume that it is satisfied.
  void execute() {
    if (direction == FORWARD) {
      v2.value = v1.value * scale.value + offset.value;
    } else {
      v1.value = (v2.value - offset.value) ~/ scale.value;
    }
  }

  /**
   * Calculate the walkabout strength, the stay flag, and, if it is
   * 'stay', the value for the current output of this constraint. Assume
   * this constraint is satisfied.
   */
  void recalculate() {
    Variable ihn = input(), out = output();
    out.walkStrength = Strength.weakest(strength, ihn.walkStrength);
    out.stay = ihn.stay && scale.stay && offset.stay;
    if (out.stay) execute();
  }
}

/**
 * Constrains two variables to have the same value.
 */
class EqualityConstraint extends BinaryConstraint {
  EqualityConstraint(Planner planner, Variable v1, Variable v2, int strength)
      : super(planner, v1, v2, strength);

  /// Enforce this constraint. Assume that it is satisfied.
  void execute() {
    output().value = input().value;
  }
}

/**
 * A constrained variable. In addition to its value, it maintain the
 * structure of the constraint graph, the current dataflow graph, and
 * various parameters of interest to the DeltaBlue incremental
 * constraint solver.
 **/
class Variable {
  ConstraintList constraints = new ConstraintList();
  Constraint determinedBy;
  int mark = 0;
  int walkStrength = WEAKEST;
  bool stay = true;
  int value;

  Variable(this.value);

  /**
   * Add the given constraint to the set of all constraints that refer
   * this variable.
   */
  void addConstraint(Constraint c) {
    constraints.add(c);
  }

  /// Removes all traces of c from this variable.
  void removeConstraint(Constraint c) {
    constraints.remove(c);
    if (determinedBy == c) determinedBy = null;
  }
}

class Planner {
  int currentMark = 0;

  /**
   * Attempt to satisfy the given constraint and, if successful,
   * incrementally update the dataflow graph.  Details: If satisfying
   * the constraint is successful, it may override a weaker constraint
   * on its output. The algorithm attempts to resatisfy that
   * constraint using some other method. This process is repeated
   * until either a) it reaches a variable that was not previously
   * determined by any constraint or b) it reaches a constraint that
   * is too weak to be satisfied using any of its methods. The
   * variables of constraints that have been processed are marked with
   * a unique mark value so that we know where we've been. This allows
   * the algorithm to avoid getting into an infinite loop even if the
   * constraint graph has an inadvertent cycle.
   */
  void incrementalAdd(Constraint c) {
    int mark = newMark();
    for (Constraint overridden = c.satisfy(mark);
        overridden != null;
        overridden = overridden.satisfy(mark));
  }

  /**
   * Entry point for retracting a constraint. Remove the given
   * constraint and incrementally update the dataflow graph.
   * Details: Retracting the given constraint may allow some currently
   * unsatisfiable downstream constraint to be satisfied. We therefore collect
   * a list of unsatisfied downstream constraints and attempt to
   * satisfy each one in turn. This list is traversed by constraint
   * strength, strongest first, as a heuristic for avoiding
   * unnecessarily adding and then overriding weak constraints.
   * Assume: [c] is satisfied.
   */
  void incrementalRemove(Constraint c) {
    Variable out = c.output();
    c.markUnsatisfied();
    c.removeFromGraph();
    ConstraintList unsatisfied = removePropagateFrom(out);
    int strength = REQUIRED;
    do {
      for (var it = unsatisfied.iterator; !it.done; it.advance()) {
        Constraint u = it.current;
        if (u.strength == strength) incrementalAdd(u);
      }
      strength = Strength.nextWeaker(strength);
    } while (strength != WEAKEST);
  }

  /// Select a previously unused mark value.
  int newMark() => ++currentMark;

  /**
   * Extract a plan for resatisfaction starting from the given source
   * constraints, usually a set of input constraints. This method
   * assumes that stay optimization is desired; the plan will contain
   * only constraints whose output variables are not stay. Constraints
   * that do no computation, such as stay and edit constraints, are
   * not included in the plan.
   * Details: The outputs of a constraint are marked when it is added
   * to the plan under construction. A constraint may be appended to
   * the plan when all its input variables are known. A variable is
   * known if either a) the variable is marked (indicating that has
   * been computed by a constraint appearing earlier in the plan), b)
   * the variable is 'stay' (i.e. it is a constant at plan execution
   * time), or c) the variable is not determined by any
   * constraint. The last provision is for past states of history
   * variables, which are not stay but which are also not computed by
   * any constraint.
   * Assume: [sources] are all satisfied.
   */
  Plan makePlan(ConstraintList sources) {
    int mark = newMark();
    Plan plan = new Plan();
    ConstraintList todo = sources;
    while (!todo.isEmpty) {
      Constraint c = todo.removeLast();
      if (c.output().mark != mark && c.inputsKnown(mark)) {
        plan.addConstraint(c);
        c.output().mark = mark;
        addConstraintsConsumingTo(c.output(), todo);
      }
    }
    return plan;
  }

  /**
   * Extract a plan for resatisfying starting from the output of the
   * given [constraints], usually a set of input constraints.
   */
  Plan extractPlanFromConstraints(ConstraintList constraints) {
    ConstraintList sources = new ConstraintList();
    for (var it = constraints.iterator; !it.done; it.advance()) {
      Constraint c = it.current;
      // if not in plan already and eligible for inclusion.
      if (c.isInput() && c.isSatisfied()) sources.add(c);
    }
    return makePlan(sources);
  }

  /**
   * Recompute the walkabout strengths and stay flags of all variables
   * downstream of the given constraint and recompute the actual
   * values of all variables whose stay flag is true. If a cycle is
   * detected, remove the given constraint and answer
   * false. Otherwise, answer true.
   * Details: Cycles are detected when a marked variable is
   * encountered downstream of the given constraint. The sender is
   * assumed to have marked the inputs of the given constraint with
   * the given mark. Thus, encountering a marked node downstream of
   * the output constraint means that there is a path from the
   * constraint's output to one of its inputs.
   */
  bool addPropagate(Constraint c, int mark) {
    ConstraintList todo = new ConstraintList()..add(c);
    while (!todo.isEmpty) {
      Constraint d = todo.removeLast();
      if (d.output().mark == mark) {
        incrementalRemove(c);
        return false;
      }
      d.recalculate();
      addConstraintsConsumingTo(d.output(), todo);
    }
    return true;
  }

  /**
   * Update the walkabout strengths and stay flags of all variables
   * downstream of the given constraint. Answer a collection of
   * unsatisfied constraints sorted in order of decreasing strength.
   */
  ConstraintList removePropagateFrom(Variable out) {
    out.determinedBy = null;
    out.walkStrength = WEAKEST;
    out.stay = true;
    ConstraintList unsatisfied = new ConstraintList();
    VariableStack todo = new VariableStack()..push(out);
    while (!todo.isEmpty) {
      Variable v = todo.pop();
      for (var it = v.constraints.iterator; !it.done; it.advance()) {
        Constraint c = it.current;
        if (!c.isSatisfied()) unsatisfied.add(c);
      }
      Constraint determining = v.determinedBy;
      for (var it = v.constraints.iterator; !it.done; it.advance()) {
        Constraint next = it.current;
        if (next != determining && next.isSatisfied()) {
          next.recalculate();
          todo.push(next.output());
        }
      }
    }
    return unsatisfied;
  }

  void addConstraintsConsumingTo(Variable v, ConstraintList coll) {
    Constraint determining = v.determinedBy;
    for (var it = v.constraints.iterator; !it.done; it.advance()) {
      Constraint c = it.current;
      if (c != determining && c.isSatisfied()) coll.add(c);
    }
  }
}

/**
 * A Plan is an ordered list of constraints to be executed in sequence
 * to resatisfy all currently satisfiable constraints in the face of
 * one or more changing inputs.
 */
class Plan {
  ConstraintList list = new ConstraintList();

  void addConstraint(Constraint c) {
    list.add(c);
  }

  void execute() {
    for (var it = list.iterator; !it.done; it.advance()) {
      it.current.execute();
    }
  }
}

/**
 * This is the standard DeltaBlue benchmark. A long chain of equality
 * constraints is constructed with a stay constraint on one end. An
 * edit constraint is then added to the opposite end and the time is
 * measured for adding and removing this constraint, and extracting
 * and executing a constraint satisfaction plan. There are two cases.
 * In case 1, the added constraint is stronger than the stay
 * constraint and values must propagate down the entire length of the
 * chain. In case 2, the added constraint is weaker than the stay
 * constraint so it cannot be accommodated. The cost in this case is,
 * of course, very low. Typical situations lie somewhere between these
 * two extremes.
 */
void chainTest(int n) {
  Planner planner = new Planner();
  Variable prev = null, first = null, last = null;
  // Build chain of n equality constraints.
  for (int i = 0; i <= n; i++) {
    Variable v = new Variable(0);
    if (prev != null) new EqualityConstraint(planner, prev, v, REQUIRED);
    if (i == 0) first = v;
    if (i == n) last = v;
    prev = v;
  }
  new StayConstraint(planner, last, STRONG_DEFAULT);
  EditConstraint edit = new EditConstraint(planner, first, PREFERRED);
  Plan plan =
      planner.extractPlanFromConstraints(new ConstraintList()..add(edit));
  for (int i = 0; i < 100; i++) {
    first.value = i;
    plan.execute();
    if (last.value != i) {
      errorChainTestFailed(i, last.value);
    }
  }
}

/**
 * This test constructs a two sets of variables related to each
 * other by a simple linear transformation (scale and offset). The
 * time is measured to change a variable on either side of the
 * mapping and to change the scale and offset factors.
 */
void projectionTest(int n) {
  Planner planner = new Planner();
  Variable scale = new Variable(10);
  Variable offset = new Variable(1000);
  Variable src = null, dst = null;

  VariableStack dests = new VariableStack();
  for (int i = 0; i < n; i++) {
    src = new Variable(i);
    dst = new Variable(i);
    dests.push(dst);
    new StayConstraint(planner, src, NORMAL);
    new ScaleConstraint(planner, src, scale, offset, dst, REQUIRED);
  }
  change(planner, src, 17);
  if (dst.value != 1170) errorProjectionFailed(1);
  change(planner, dst, 1050);
  if (src.value != 5) errorProjectionFailed(2);
  change(planner, scale, 5);
  int expected = (n - 2) * 5 + 1000;
  for (var vars = dests.clone()..pop(); !vars.isEmpty;) {
    if (vars.pop().value != expected) errorProjectionFailed(3);
    expected -= 5;
  }
  change(planner, offset, 2000);
  expected = (n - 2) * 5 + 2000;
  for (var vars = dests.clone()..pop(); !vars.isEmpty;) {
    if (vars.pop().value != expected) errorProjectionFailed(4);
    expected -= 5;
  }
}

void change(Planner planner, Variable v, int newValue) {
  EditConstraint edit = new EditConstraint(planner, v, PREFERRED);
  Plan plan =
      planner.extractPlanFromConstraints(new ConstraintList()..add(edit));
  for (int i = 0; i < 10; i++) {
    v.value = newValue;
    plan.execute();
  }
  edit.destroyConstraint();
}

// The classes below are the added replacements for the Dart built-in lists:

class VariableStack {
  VariableStackElement top;

  void push(Variable variable) {
    top = new VariableStackElement(variable, top);
  }

  Variable pop() {
    Variable topVar = top.variable;
    top = top.next;
    return topVar;
  }

  bool get isEmpty => top == null;

  VariableStack clone() => new VariableStack()..top = top;
}

class VariableStackElement {
  final Variable variable;
  final VariableStackElement next;

  VariableStackElement(this.variable, this.next);
}

class ConstraintList {
  ConstraintListElement dummy;

  ConstraintList() {
    dummy = new ConstraintListElement(null);
    dummy.next = dummy;
    dummy.prev = dummy;
  }

  bool get isEmpty => dummy.next == dummy;

  void add(Constraint constraint) {
    ConstraintListElement element = new ConstraintListElement(constraint);
    element.prev = dummy.prev;
    element.next = dummy;
    dummy.prev.next = element;
    dummy.prev = element;
  }

  Constraint removeLast() {
    assert(!isEmpty);
    ConstraintListElement last = dummy.prev;
    last.remove();
    return last.value;
  }

  void remove(Constraint constraint) {
    for (var elem = dummy.next; elem != dummy; elem = elem.next) {
      if (elem.value == constraint) {
        elem.remove();
        return;
      }
    }
  }

  ConstraintListIterator get iterator => new ConstraintListIterator(this);
}

class ConstraintListElement {
  final Constraint value;
  ConstraintListElement next;
  ConstraintListElement prev;

  ConstraintListElement(this.value);

  void remove() {
    prev.next = next;
    next.prev = prev;
  }
}

class ConstraintListIterator {
  ConstraintListElement currentElement;

  ConstraintListIterator(ConstraintList list)
      : currentElement = list.dummy.next;

  Constraint get current => currentElement.value;

  bool get done => currentElement.value == null;

  void advance() => currentElement = currentElement.next;
}

// If an error occurs, one of the following functions is called instead of the
// usual printing. These should be implemented to signal the error in an
// appropriate way.

@pragma("vm:never-inline")
void errorRequiredConstraint() {
  // Could not satisfy a required constraint!
}

@pragma("vm:never-inline")
void errorCycleEncountered() {
  // Cycle encountered.
}

@pragma("vm:never-inline")
void errorChainTestFailed(int a, int b) {
  // Chain test failed:
  // Expected last value to be $a but it was $b.
}

@pragma("vm:never-inline")
void errorProjectionFailed(int n) {
  // Projection $n failed.
}
