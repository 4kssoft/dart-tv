// Copyright (c) 2014, the Dart project authors. Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'package:analysis_server/src/services/search/hierarchy.dart';
import 'package:analysis_server/src/services/search/search_engine_internal.dart';
import 'package:analyzer/dart/element/element.dart';
import 'package:test/test.dart';
import 'package:test_reflective_loader/test_reflective_loader.dart';

import '../../abstract_single_unit.dart';

void main() {
  defineReflectiveSuite(() {
    defineReflectiveTests(HierarchyTest);
  });
}

@reflectiveTest
class HierarchyTest extends AbstractSingleUnitTest {
  late SearchEngineImpl searchEngine;

  @override
  void setUp() {
    super.setUp();
    searchEngine = SearchEngineImpl([driverFor(testFile)]);
  }

  Future<void> test_getClassMembers() async {
    await _indexTestUnit('''
class A {
  A() {}
  var ma1;
  ma2() {}
}
class B extends A {
  B() {}
  B.named() {}
  var mb1;
  mb2() {}
}
''');
    {
      var classA = findElement.class_('A');
      var members = getClassMembers(classA);
      expect(members.map((e) => e.name), unorderedEquals(['ma1', 'ma2']));
    }
    {
      var classB = findElement.class_('B');
      var members = getClassMembers(classB);
      expect(members.map((e) => e.name), unorderedEquals(['mb1', 'mb2']));
    }
  }

  Future<void> test_getHierarchyMembers_constructors() async {
    await _indexTestUnit('''
class A {
  A() {}
}
class B extends A {
  B() {}
}
''');
    var classA = findElement.class_('A');
    var classB = findElement.class_('B');
    ClassMemberElement memberA = classA.constructors[0];
    ClassMemberElement memberB = classB.constructors[0];
    var futureA = getHierarchyMembers(searchEngine, memberA).then((members) {
      expect(members, unorderedEquals([memberA]));
    });
    var futureB = getHierarchyMembers(searchEngine, memberB).then((members) {
      expect(members, unorderedEquals([memberB]));
    });
    await Future.wait([futureA, futureB]);
  }

  Future<void> test_getHierarchyMembers_fields() async {
    await _indexTestUnit('''
class A {
  int? foo;
}
class B extends A {
  get foo => null;
}
class C extends B {
  set foo(x) {}
}
class D {
  int? foo;
}
''');
    var classA = findElement.class_('A');
    var classB = findElement.class_('B');
    var classC = findElement.class_('C');
    var classD = findElement.class_('D');
    ClassMemberElement memberA = classA.fields[0];
    ClassMemberElement memberB = classB.fields[0];
    ClassMemberElement memberC = classC.fields[0];
    ClassMemberElement memberD = classD.fields[0];
    var futureA = getHierarchyMembers(searchEngine, memberA).then((members) {
      expect(members, unorderedEquals([memberA, memberB, memberC]));
    });
    var futureB = getHierarchyMembers(searchEngine, memberB).then((members) {
      expect(members, unorderedEquals([memberA, memberB, memberC]));
    });
    var futureC = getHierarchyMembers(searchEngine, memberC).then((members) {
      expect(members, unorderedEquals([memberA, memberB, memberC]));
    });
    var futureD = getHierarchyMembers(searchEngine, memberD).then((members) {
      expect(members, unorderedEquals([memberD]));
    });
    await Future.wait([futureA, futureB, futureC, futureD]);
  }

  Future<void> test_getHierarchyMembers_fields_static() async {
    await _indexTestUnit('''
class A {
  static int? foo;
}
class B extends A {
  static get foo => null;
}
class C extends B {
  static set foo(x) {}
}
''');
    var classA = findElement.class_('A');
    var classB = findElement.class_('B');
    var classC = findElement.class_('C');
    ClassMemberElement memberA = classA.fields[0];
    ClassMemberElement memberB = classB.fields[0];
    ClassMemberElement memberC = classC.fields[0];
    {
      var members = await getHierarchyMembers(searchEngine, memberA);
      expect(members, unorderedEquals([memberA]));
    }
    {
      var members = await getHierarchyMembers(searchEngine, memberB);
      expect(members, unorderedEquals([memberB]));
    }
    {
      var members = await getHierarchyMembers(searchEngine, memberC);
      expect(members, unorderedEquals([memberC]));
    }
  }

  Future<void> test_getHierarchyMembers_methods() async {
    await _indexTestUnit('''
class A {
  foo() {}
}
class B extends A {
  foo() {}
}
class C extends B {
  foo() {}
}
class D {
  foo() {}
}
class E extends D {
  foo() {}
}
''');
    var classA = findElement.class_('A');
    var classB = findElement.class_('B');
    var classC = findElement.class_('C');
    var classD = findElement.class_('D');
    var classE = findElement.class_('E');
    ClassMemberElement memberA = classA.methods[0];
    ClassMemberElement memberB = classB.methods[0];
    ClassMemberElement memberC = classC.methods[0];
    ClassMemberElement memberD = classD.methods[0];
    ClassMemberElement memberE = classE.methods[0];
    var futureA = getHierarchyMembers(searchEngine, memberA).then((members) {
      expect(members, unorderedEquals([memberA, memberB, memberC]));
    });
    var futureB = getHierarchyMembers(searchEngine, memberB).then((members) {
      expect(members, unorderedEquals([memberA, memberB, memberC]));
    });
    var futureC = getHierarchyMembers(searchEngine, memberC).then((members) {
      expect(members, unorderedEquals([memberA, memberB, memberC]));
    });
    var futureD = getHierarchyMembers(searchEngine, memberD).then((members) {
      expect(members, unorderedEquals([memberD, memberE]));
    });
    var futureE = getHierarchyMembers(searchEngine, memberE).then((members) {
      expect(members, unorderedEquals([memberD, memberE]));
    });
    await Future.wait([futureA, futureB, futureC, futureD, futureE]);
  }

  Future<void> test_getHierarchyMembers_methods_static() async {
    await _indexTestUnit('''
class A {
  static foo() {}
}
class B extends A {
  static foo() {}
}
''');
    var classA = findElement.class_('A');
    var classB = findElement.class_('B');
    ClassMemberElement memberA = classA.methods[0];
    ClassMemberElement memberB = classB.methods[0];
    {
      var members = await getHierarchyMembers(searchEngine, memberA);
      expect(members, unorderedEquals([memberA]));
    }
    {
      var members = await getHierarchyMembers(searchEngine, memberB);
      expect(members, unorderedEquals([memberB]));
    }
  }

  Future<void> test_getHierarchyMembers_withInterfaces() async {
    await _indexTestUnit('''
class A {
  foo() {}
}
class B implements A {
  foo() {}
}
abstract class C implements A {
}
class D extends C {
  foo() {}
}
class E {
  foo() {}
}
''');
    var classA = findElement.class_('A');
    var classB = findElement.class_('B');
    var classD = findElement.class_('D');
    ClassMemberElement memberA = classA.methods[0];
    ClassMemberElement memberB = classB.methods[0];
    ClassMemberElement memberD = classD.methods[0];
    var futureA = getHierarchyMembers(searchEngine, memberA).then((members) {
      expect(members, unorderedEquals([memberA, memberB, memberD]));
    });
    var futureB = getHierarchyMembers(searchEngine, memberB).then((members) {
      expect(members, unorderedEquals([memberA, memberB, memberD]));
    });
    var futureD = getHierarchyMembers(searchEngine, memberD).then((members) {
      expect(members, unorderedEquals([memberA, memberB, memberD]));
    });
    await Future.wait([futureA, futureB, futureD]);
  }

  Future<void> test_getHierarchyNamedParameters() async {
    await _indexTestUnit('''
class A {
  foo({p}) {}
}
class B extends A {
  foo({p}) {}
}
class C extends B {
  foo({p}) {}
}
class D {
  foo({p}) {}
}
class E extends D {
  foo({p}) {}
}
''');
    var classA = findElement.class_('A');
    var classB = findElement.class_('B');
    var classC = findElement.class_('C');
    var classD = findElement.class_('D');
    var classE = findElement.class_('E');
    var parameterA = classA.methods[0].parameters[0];
    var parameterB = classB.methods[0].parameters[0];
    var parameterC = classC.methods[0].parameters[0];
    var parameterD = classD.methods[0].parameters[0];
    var parameterE = classE.methods[0].parameters[0];

    {
      var result = await getHierarchyNamedParameters(searchEngine, parameterA);
      expect(result, unorderedEquals([parameterA, parameterB, parameterC]));
    }

    {
      var result = await getHierarchyNamedParameters(searchEngine, parameterB);
      expect(result, unorderedEquals([parameterA, parameterB, parameterC]));
    }

    {
      var result = await getHierarchyNamedParameters(searchEngine, parameterC);
      expect(result, unorderedEquals([parameterA, parameterB, parameterC]));
    }

    {
      var result = await getHierarchyNamedParameters(searchEngine, parameterD);
      expect(result, unorderedEquals([parameterD, parameterE]));
    }

    {
      var result = await getHierarchyNamedParameters(searchEngine, parameterE);
      expect(result, unorderedEquals([parameterD, parameterE]));
    }
  }

  Future<void> test_getHierarchyNamedParameters_invalid_missing() async {
    verifyNoTestUnitErrors = false;
    await _indexTestUnit('''
class A {
  foo({p}) {}
}
class B extends A {
  foo() {}
}
''');
    var classA = findElement.class_('A');
    var parameterA = classA.methods[0].parameters[0];

    var result = await getHierarchyNamedParameters(searchEngine, parameterA);
    expect(result, unorderedEquals([parameterA]));
  }

  Future<void> test_getHierarchyNamedParameters_invalid_notNamed() async {
    verifyNoTestUnitErrors = false;
    await _indexTestUnit('''
class A {
  foo({p}) {}
}
class B extends A {
  foo(p) {}
}
''');
    var classA = findElement.class_('A');
    var parameterA = classA.methods[0].parameters[0];

    var result = await getHierarchyNamedParameters(searchEngine, parameterA);
    expect(result, unorderedEquals([parameterA]));
  }

  Future<void> test_getMembers() async {
    await _indexTestUnit('''
class A {
  A() {}
  var ma1;
  ma2() {}
}
class B extends A {
  B() {}
  B.named() {}
  var mb1;
  mb2() {}
}
''');
    {
      var classA = findElement.class_('A');
      var members = getMembers(classA);
      expect(
          members.map((e) => e.name),
          unorderedEquals([
            'ma1',
            'ma2',
            '==',
            'toString',
            'hashCode',
            'noSuchMethod',
            'runtimeType',
            'hash',
            'hashAll',
            'hashAllUnordered',
          ]));
    }
    {
      var classB = findElement.class_('B');
      var members = getMembers(classB);
      expect(
          members.map((e) => e.name),
          unorderedEquals([
            'mb1',
            'mb2',
            'ma1',
            'ma2',
            '==',
            'toString',
            'hashCode',
            'noSuchMethod',
            'runtimeType',
            'hash',
            'hashAll',
            'hashAllUnordered',
          ]));
    }
  }

  Future<void> test_getSuperClasses() async {
    await _indexTestUnit('''
class A {}
class B extends A {}
class C extends B {}
class D extends B implements A {}
class M {}
class E extends A with M {}
class F implements A {}
''');
    var classA = findElement.class_('A');
    var classB = findElement.class_('B');
    var classC = findElement.class_('C');
    var classD = findElement.class_('D');
    var classE = findElement.class_('E');
    var classF = findElement.class_('F');
    var objectElement = classA.supertype!.element;
    // Object
    {
      var supers = getSuperClasses(objectElement);
      expect(supers, isEmpty);
    }
    // A
    {
      var supers = getSuperClasses(classA);
      expect(supers, unorderedEquals([objectElement]));
    }
    // B
    {
      var supers = getSuperClasses(classB);
      expect(supers, unorderedEquals([objectElement, classA]));
    }
    // C
    {
      var supers = getSuperClasses(classC);
      expect(supers, unorderedEquals([objectElement, classA, classB]));
    }
    // D
    {
      var supers = getSuperClasses(classD);
      expect(supers, unorderedEquals([objectElement, classA, classB]));
    }
    // E
    {
      var supers = getSuperClasses(classE);
      expect(supers, unorderedEquals([objectElement, classA]));
    }
    // F
    {
      var supers = getSuperClasses(classF);
      expect(supers, unorderedEquals([objectElement, classA]));
    }
  }

  Future<void> test_getSuperClasses_superclassConstraints() async {
    await _indexTestUnit('''
class A {}
class B extends A {}
class C {}

mixin M1 on A {}
mixin M2 on B {}
mixin M3 on M1 {}
mixin M4 on M2 {}
mixin M5 on A, C {}
''');
    var a = findElement.class_('A');
    var b = findElement.class_('B');
    var c = findElement.class_('C');
    var m1 = findElement.mixin('M1');
    var m2 = findElement.mixin('M2');
    var m3 = findElement.mixin('M3');
    var m4 = findElement.mixin('M4');
    var m5 = findElement.mixin('M5');
    var object = a.supertype!.element;

    _assertSuperClasses(object, []);
    _assertSuperClasses(a, [object]);
    _assertSuperClasses(b, [object, a]);
    _assertSuperClasses(c, [object]);

    _assertSuperClasses(m1, [object, a]);
    _assertSuperClasses(m2, [object, a, b]);
    _assertSuperClasses(m3, [object, a, m1]);
    _assertSuperClasses(m4, [object, a, b, m2]);
    _assertSuperClasses(m5, [object, a, c]);
  }

  void _assertSuperClasses(ClassElement element, List<ClassElement> expected) {
    var supers = getSuperClasses(element);
    expect(supers, unorderedEquals(expected));
  }

  Future<void> _indexTestUnit(String code) async {
    await resolveTestCode(code);
  }
}
