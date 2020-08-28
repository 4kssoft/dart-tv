// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import '../../common_elements.dart' show CommonElements;
import '../../constants/values.dart';
import '../../elements/entities.dart';
import '../../elements/names.dart';
import '../../elements/types.dart' show DartType, InterfaceType;
import '../../ir/static_type.dart';
import '../../universe/selector.dart';
import '../../world.dart';
import '../abstract_value_domain.dart';

/// This class is used as an API by the powerset abstract value domain to help
/// implement some queries. It stores the bitmasks as integers and has the
/// advantage that the operations needed are relatively fast. This will pack
/// multiple powerset domains into a single integer.
class PowersetBitsDomain {
  final JClosedWorld _closedWorld;

  static const int _trueIndex = 0;
  static const int _falseIndex = 1;
  static const int _nullIndex = 2;
  static const int _otherIndex = 3;

  static const int _maxIndex = _otherIndex;
  static const List<int> _singletonIndices = [
    _trueIndex,
    _falseIndex,
    _nullIndex,
  ];

  const PowersetBitsDomain(this._closedWorld);

  CommonElements get commonElements => _closedWorld.commonElements;

  int get trueMask => 1 << _trueIndex;
  int get falseMask => 1 << _falseIndex;
  int get nullMask => 1 << _nullIndex;
  int get otherMask => 1 << _otherIndex;
  int get boolMask => trueMask | falseMask;
  int get boolOrNullMask => boolMask | nullMask;
  int get nullOrOtherMask => nullMask | otherMask;
  int get boolNullOtherMask => boolOrNullMask | otherMask;
  int get preciseMask => _singletonIndices.fold(
      powersetBottom, (mask, index) => mask | 1 << index);

  int get powersetBottom => 0;
  int get powersetTop => (1 << _maxIndex + 1) - 1;

  bool isPotentiallyBoolean(int value) => (value & boolMask) != 0;
  bool isPotentiallyNull(int value) => (value & nullMask) != 0;
  bool isPotentiallyOther(int value) => (value & otherMask) != 0;

  bool isDefinitelyTrue(int value) => value == trueMask;
  bool isDefinitelyFalse(int value) => value == falseMask;
  bool isDefinitelyNull(int value) => value == nullMask;
  bool isSingleton(int value) =>
      isDefinitelyTrue(value) ||
      isDefinitelyFalse(value) ||
      isDefinitelyNull(value);

  /// Returns `true` if only singleton bits are set and `false` otherwise.
  bool isPrecise(int value) => value & ~preciseMask == 0;

  AbstractBool isOther(int value) =>
      AbstractBool.maybeOrFalse(isPotentiallyOther(value));

  AbstractBool isIn(int subset, int superset) {
    if (union(subset, superset) == superset) {
      if (isPrecise(superset)) return AbstractBool.True;
    } else {
      if (isPrecise(subset)) return AbstractBool.False;
    }
    return AbstractBool.Maybe;
  }

  AbstractBool needsNoSuchMethodHandling(int receiver, Selector selector) =>
      AbstractBool.Maybe;

  AbstractBool isTargetingMember(
          int receiver, MemberEntity member, Name name) =>
      AbstractBool.Maybe;

  int computeReceiver(Iterable<MemberEntity> members) {
    return powersetTop;
  }

  // TODO(coam): This currently returns null if we are not sure if it's a primitive.
  // It could be improved because we can also tell when we're certain it's not a primitive.
  PrimitiveConstantValue getPrimitiveValue(int value) {
    if (isDefinitelyTrue(value)) {
      return TrueConstantValue();
    }
    if (isDefinitelyFalse(value)) {
      return FalseConstantValue();
    }
    if (isDefinitelyNull(value)) {
      return NullConstantValue();
    }
    return null;
  }

  int createPrimitiveValue(PrimitiveConstantValue value) {
    return computeAbstractValueForConstant(value);
  }

  // TODO(coam): Same as getPrimitiveValue above.
  bool isPrimitiveValue(int value) => isSingleton(value);

  int computeAbstractValueForConstant(ConstantValue value) {
    if (value.isTrue) {
      return trueMask;
    }
    if (value.isFalse) {
      return falseMask;
    }
    if (value.isNull) {
      return nullMask;
    }
    return otherMask;
  }

  AbstractBool areDisjoint(int a, int b) {
    int overlap = intersection(a, b);
    if (overlap == powersetBottom) return AbstractBool.True;
    if (isPrecise(overlap)) return AbstractBool.False;
    return AbstractBool.Maybe;
  }

  int intersection(int a, int b) {
    return a & b;
  }

  int union(int a, int b) {
    return a | b;
  }

  AbstractBool isPrimitiveOrNull(int value) => isPrimitive(excludeNull(value));

  AbstractBool isStringOrNull(int value) => isString(excludeNull(value));

  AbstractBool isString(int value) => isOther(value);

  AbstractBool isBooleanOrNull(int value) => isBoolean(excludeNull(value));

  AbstractBool isBoolean(int value) {
    if (!isPotentiallyBoolean(value)) return AbstractBool.False;
    if (value & ~boolMask == 0) return AbstractBool.True;
    return AbstractBool.Maybe;
  }

  AbstractBool isDoubleOrNull(int value) => isDouble(excludeNull(value));

  AbstractBool isDouble(int value) => isOther(value);

  AbstractBool isNumberOrNull(int value) => isNumber(excludeNull(value));

  AbstractBool isNumber(int value) => isOther(value);

  AbstractBool isIntegerOrNull(int value) => isInteger(excludeNull(value));

  AbstractBool isPositiveIntegerOrNull(int value) =>
      isPositiveInteger(excludeNull(value));

  AbstractBool isPositiveInteger(int value) => isOther(value);

  AbstractBool isUInt31(int value) => isOther(value);

  AbstractBool isUInt32(int value) => isOther(value);

  AbstractBool isInteger(int value) => isOther(value);

  AbstractBool isInterceptor(int value) => AbstractBool.Maybe;

  AbstractBool isPrimitiveString(int value) => isOther(value);

  AbstractBool isArray(int value) => isOther(value);

  AbstractBool isMutableIndexable(int value) => isOther(value);

  AbstractBool isMutableArray(int value) => isOther(value);

  AbstractBool isExtendableArray(int value) => isOther(value);

  AbstractBool isFixedArray(int value) => isOther(value);

  AbstractBool isIndexablePrimitive(int value) => isOther(value);

  AbstractBool isPrimitiveArray(int value) => isOther(value);

  AbstractBool isPrimitiveBoolean(int value) {
    if (isDefinitelyTrue(value) || isDefinitelyFalse(value)) {
      return AbstractBool.True;
    }
    if (!isPotentiallyBoolean(value)) return AbstractBool.False;
    return AbstractBool.Maybe;
  }

  AbstractBool isPrimitiveNumber(int value) => isOther(value);

  AbstractBool isPrimitive(int value) =>
      AbstractBool.trueOrMaybe(isSingleton(value));

  AbstractBool isNull(int value) => isDefinitelyNull(value)
      ? AbstractBool.True
      : (isPotentiallyNull(value) ? AbstractBool.Maybe : AbstractBool.False);

  AbstractBool isExactOrNull(int value) => AbstractBool.Maybe;

  AbstractBool isExact(int value) => AbstractBool.Maybe;

  AbstractBool isEmpty(int value) {
    if (value == powersetBottom) return AbstractBool.True;
    if (isPrecise(value)) return AbstractBool.False;
    return AbstractBool.Maybe;
  }

  AbstractBool isInstanceOf(int value, ClassEntity cls) => AbstractBool.Maybe;

  AbstractBool isInstanceOfOrNull(int value, ClassEntity cls) =>
      AbstractBool.Maybe;

  AbstractBool containsAll(int value) =>
      AbstractBool.maybeOrFalse(value == powersetTop);

  AbstractBool containsOnlyType(int value, ClassEntity cls) =>
      AbstractBool.Maybe;

  AbstractBool containsType(int value, ClassEntity cls) => AbstractBool.Maybe;

  int includeNull(int value) {
    return value | nullMask;
  }

  int excludeNull(int value) {
    return value & ~nullMask;
  }

  AbstractBool couldBeTypedArray(int value) => isOther(value);

  AbstractBool isTypedArray(int value) => AbstractBool.Maybe;

  bool isBoolSubtype(ClassEntity cls) {
    return cls == commonElements.jsBoolClass || cls == commonElements.boolClass;
  }

  int createNullableSubtype(ClassEntity cls) {
    if (isBoolSubtype(cls)) {
      return boolOrNullMask;
    }
    return nullOrOtherMask;
  }

  int createNonNullSubtype(ClassEntity cls) {
    if (isBoolSubtype(cls)) {
      return boolMask;
    }
    return otherMask;
  }

  int createNonNullSubclass(ClassEntity cls) {
    if (isBoolSubtype(cls)) {
      return boolMask;
    }
    return otherMask;
  }

  int createNullableExact(ClassEntity cls) {
    if (isBoolSubtype(cls)) {
      return boolOrNullMask;
    }
    return nullOrOtherMask;
  }

  int createNonNullExact(ClassEntity cls) {
    if (isBoolSubtype(cls)) {
      return boolMask;
    }
    return otherMask;
  }

  int createFromStaticType(DartType type,
      {ClassRelation classRelation = ClassRelation.subtype, bool nullable}) {
    // TODO(coam): This only works for bool
    int bits = otherMask;
    if (type is InterfaceType && isBoolSubtype(type.element)) {
      bits = boolMask;
    }
    if (nullable) {
      bits = bits | nullMask;
    }
    return bits;
  }

  int get asyncStarStreamType => powersetTop;

  int get asyncFutureType => powersetTop;

  int get syncStarIterableType => powersetTop;

  int get emptyType => powersetBottom;

  int get constMapType => otherMask;

  int get constSetType => otherMask;

  int get constListType => otherMask;

  int get positiveIntType => otherMask;

  int get uint32Type => otherMask;

  int get uint31Type => otherMask;

  int get fixedListType => otherMask;

  int get growableListType => otherMask;

  int get nullType => nullMask;

  int get nonNullType => otherMask;

  int get mapType => otherMask;

  int get setType => otherMask;

  int get listType => otherMask;

  int get stringType => otherMask;

  int get numType => otherMask;

  int get doubleType => otherMask;

  int get intType => otherMask;

  int get boolType => boolMask;

  int get functionType => otherMask;

  int get typeType => otherMask;
}
