// Copyright (c) 2019, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.
//
// Dart test program for testing dart:ffi function pointers.
//
// VMOptions=
// VMOptions=--deterministic --optimization-counter-threshold=10
// VMOptions=--use-slow-path
// VMOptions=--use-slow-path --stacktrace-every=100
// VMOptions=--write-protect-code --no-dual-map-code
// VMOptions=--write-protect-code --no-dual-map-code --use-slow-path
// VMOptions=--write-protect-code --no-dual-map-code --stacktrace-every=100
// SharedObjects=ffi_test_functions

library FfiTest;

import 'dart:ffi' as ffi;
import 'dart:ffi' show Pointer;

import 'dylib_utils.dart';

import "package:expect/expect.dart";

void main() {
  for (int i = 0; i < 100; ++i) {
    testNativeFunctionFromCast();
    testNativeFunctionFromLookup();
    test64bitInterpretations();
    testExtension();
    testTruncation();
    testNativeFunctionDoubles();
    testNativeFunctionFloats();
    testNativeFunctionManyArguments1();
    testNativeFunctionManyArguments2();
    testNativeFunctionManyArguments3();
    testNativeFunctionManyArguments4();
    testNativeFunctionPointer();
    testNullInt();
    testNullDouble();
    testNullManyArgs();
    testNullPointers();
    testFloatRounding();
    testVoidReturn();
    testNoArgs();
    testException();
  }
}

ffi.DynamicLibrary ffiTestFunctions =
    dlopenPlatformSpecific("ffi_test_functions");

typedef NativeBinaryOp = ffi.Int32 Function(ffi.Int32, ffi.Int32);
typedef UnaryOp = int Function(int);
typedef BinaryOp = int Function(int, int);
typedef GenericBinaryOp<T> = int Function(int, T);

void testNativeFunctionFromCast() {
  ffi.Pointer<ffi.IntPtr> p1 = Pointer.allocate();
  ffi.Pointer<ffi.NativeFunction<NativeBinaryOp>> p2 = p1.cast();
  p2.asFunction<BinaryOp>();
  p2.asFunction<GenericBinaryOp<int>>();
  p1.free();
}

typedef NativeQuadOpSigned = ffi.Int64 Function(
    ffi.Int8, ffi.Int16, ffi.Int32, ffi.Int64);
typedef QuadOp = int Function(int, int, int, int);
typedef NativeQuadOpUnsigned = ffi.Uint64 Function(
    ffi.Uint8, ffi.Uint16, ffi.Uint32, ffi.Uint64);

BinaryOp sumPlus42 =
    ffiTestFunctions.lookupFunction<NativeBinaryOp, BinaryOp>("SumPlus42");

QuadOp intComputation = ffiTestFunctions
    .lookupFunction<NativeQuadOpSigned, QuadOp>("IntComputation");

void testNativeFunctionFromLookup() {
  Expect.equals(49, sumPlus42(3, 4));

  Expect.equals(625, intComputation(125, 250, 500, 1000));

  Expect.equals(
      0x7FFFFFFFFFFFFFFF, intComputation(0, 0, 0, 0x7FFFFFFFFFFFFFFF));
  Expect.equals(
      -0x8000000000000000, intComputation(0, 0, 0, -0x8000000000000000));
}

typedef NativeReturnMaxUint8 = ffi.Uint8 Function();
int Function() returnMaxUint8 = ffiTestFunctions
    .lookup("ReturnMaxUint8")
    .cast<ffi.NativeFunction<NativeReturnMaxUint8>>()
    .asFunction();

typedef NativeReturnMaxUint16 = ffi.Uint16 Function();
int Function() returnMaxUint16 = ffiTestFunctions
    .lookup("ReturnMaxUint16")
    .cast<ffi.NativeFunction<NativeReturnMaxUint16>>()
    .asFunction();

typedef NativeReturnMaxUint32 = ffi.Uint32 Function();
int Function() returnMaxUint32 = ffiTestFunctions
    .lookup("ReturnMaxUint32")
    .cast<ffi.NativeFunction<NativeReturnMaxUint32>>()
    .asFunction();

typedef NativeReturnMinInt8 = ffi.Int8 Function();
int Function() returnMinInt8 = ffiTestFunctions
    .lookup("ReturnMinInt8")
    .cast<ffi.NativeFunction<NativeReturnMinInt8>>()
    .asFunction();

typedef NativeReturnMinInt16 = ffi.Int16 Function();
int Function() returnMinInt16 = ffiTestFunctions
    .lookup("ReturnMinInt16")
    .cast<ffi.NativeFunction<NativeReturnMinInt16>>()
    .asFunction();

typedef NativeReturnMinInt32 = ffi.Int32 Function();
int Function() returnMinInt32 = ffiTestFunctions
    .lookup("ReturnMinInt32")
    .cast<ffi.NativeFunction<NativeReturnMinInt32>>()
    .asFunction();

typedef NativeTakeMaxUint8 = ffi.IntPtr Function(ffi.Uint8);
int Function(int) takeMaxUint8 = ffiTestFunctions
    .lookup("TakeMaxUint8")
    .cast<ffi.NativeFunction<NativeTakeMaxUint8>>()
    .asFunction();

typedef NativeTakeMaxUint16 = ffi.IntPtr Function(ffi.Uint16);
int Function(int) takeMaxUint16 = ffiTestFunctions
    .lookup("TakeMaxUint16")
    .cast<ffi.NativeFunction<NativeTakeMaxUint16>>()
    .asFunction();

typedef NativeTakeMaxUint32 = ffi.IntPtr Function(ffi.Uint32);
int Function(int) takeMaxUint32 = ffiTestFunctions
    .lookup("TakeMaxUint32")
    .cast<ffi.NativeFunction<NativeTakeMaxUint32>>()
    .asFunction();

typedef NativeTakeMinInt8 = ffi.IntPtr Function(ffi.Int8);
int Function(int) takeMinInt8 = ffiTestFunctions
    .lookup("TakeMinInt8")
    .cast<ffi.NativeFunction<NativeTakeMinInt8>>()
    .asFunction();

typedef NativeTakeMinInt16 = ffi.IntPtr Function(ffi.Int16);
int Function(int) takeMinInt16 = ffiTestFunctions
    .lookup("TakeMinInt16")
    .cast<ffi.NativeFunction<NativeTakeMinInt16>>()
    .asFunction();

typedef NativeTakeMinInt32 = ffi.IntPtr Function(ffi.Int32);
int Function(int) takeMinInt32 = ffiTestFunctions
    .lookup("TakeMinInt32")
    .cast<ffi.NativeFunction<NativeTakeMinInt32>>()
    .asFunction();

void testExtension() {
  Expect.equals(returnMaxUint8(), 0xff);
  Expect.equals(returnMaxUint16(), 0xffff);
  Expect.equals(returnMaxUint32(), 0xffffffff);
  Expect.equals(returnMinInt8(), -0x80);
  Expect.equals(returnMinInt16(), -0x8000);
  Expect.equals(returnMinInt32(), -0x80000000);

  Expect.equals(takeMaxUint8(0xff), 1);
  Expect.equals(takeMaxUint16(0xffff), 1);
  Expect.equals(takeMaxUint32(0xffffffff), 1);
  Expect.equals(takeMinInt8(0x80), 1);
  Expect.equals(takeMinInt16(0x8000), 1);
  Expect.equals(takeMinInt32(0x80000000), 1);
}

QuadOp uintComputation = ffiTestFunctions
    .lookupFunction<NativeQuadOpUnsigned, QuadOp>("UintComputation");

void test64bitInterpretations() {
  // 2 ^ 63 - 1
  Expect.equals(
      0x7FFFFFFFFFFFFFFF, uintComputation(0, 0, 0, 0x7FFFFFFFFFFFFFFF));
  // -2 ^ 63 interpreted as 2 ^ 63
  Expect.equals(
      -0x8000000000000000, uintComputation(0, 0, 0, -0x8000000000000000));
  // -1 interpreted as 2 ^ 64 - 1
  Expect.equals(-1, uintComputation(0, 0, 0, -1));
}

typedef NativeSenaryOp = ffi.Int64 Function(
    ffi.Int8, ffi.Int16, ffi.Int32, ffi.Uint8, ffi.Uint16, ffi.Uint32);
typedef SenaryOp = int Function(int, int, int, int, int, int);

SenaryOp sumSmallNumbers = ffiTestFunctions
    .lookupFunction<NativeSenaryOp, SenaryOp>("SumSmallNumbers");

void testTruncation() {
  sumSmallNumbers(128, 0, 0, 0, 0, 0);
  sumSmallNumbers(-129, 0, 0, 0, 0, 0);
  sumSmallNumbers(0, 0, 0, 256, 0, 0);
  sumSmallNumbers(0, 0, 0, -1, 0, 0);

  sumSmallNumbers(0, 0x8000, 0, 0, 0, 0);
  sumSmallNumbers(0, 0xFFFFFFFFFFFF7FFF, 0, 0, 0, 0);
  sumSmallNumbers(0, 0, 0, 0, 0x10000, 0);
  sumSmallNumbers(0, 0, 0, 0, -1, 0);

  Expect.equals(0xFFFFFFFF80000000, sumSmallNumbers(0, 0, 0x80000000, 0, 0, 0));
  Expect.equals(
      0x000000007FFFFFFF, sumSmallNumbers(0, 0, 0xFFFFFFFF7FFFFFFF, 0, 0, 0));
  Expect.equals(0, sumSmallNumbers(0, 0, 0, 0, 0, 0x100000000));
  Expect.equals(0xFFFFFFFF, sumSmallNumbers(0, 0, 0, 0, 0, -1));
}

typedef NativeDoubleUnaryOp = ffi.Double Function(ffi.Double);
typedef DoubleUnaryOp = double Function(double);

DoubleUnaryOp times1_337Double = ffiTestFunctions
    .lookupFunction<NativeDoubleUnaryOp, DoubleUnaryOp>("Times1_337Double");

void testNativeFunctionDoubles() {
  Expect.approxEquals(2.0 * 1.337, times1_337Double(2.0));
}

typedef NativeFloatUnaryOp = ffi.Float Function(ffi.Float);

DoubleUnaryOp times1_337Float = ffiTestFunctions
    .lookupFunction<NativeFloatUnaryOp, DoubleUnaryOp>("Times1_337Float");

void testNativeFunctionFloats() {
  Expect.approxEquals(1337.0, times1_337Float(1000.0));
}

typedef NativeDecenaryOp = ffi.IntPtr Function(
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr);
typedef DecenaryOp = int Function(
    int, int, int, int, int, int, int, int, int, int);

DecenaryOp sumManyInts = ffiTestFunctions
    .lookupFunction<NativeDecenaryOp, DecenaryOp>("SumManyInts");

void testNativeFunctionManyArguments1() {
  Expect.equals(55, sumManyInts(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
}

typedef NativeUndenaryOp = ffi.IntPtr Function(
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr,
    ffi.IntPtr);
typedef UndenaryOp = int Function(
    int, int, int, int, int, int, int, int, int, int, int);

UndenaryOp sumManyIntsOdd = ffiTestFunctions
    .lookupFunction<NativeUndenaryOp, UndenaryOp>("SumManyIntsOdd");

void testNativeFunctionManyArguments4() {
  Expect.equals(66, sumManyIntsOdd(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11));
}

typedef NativeDoubleDecenaryOp = ffi.Double Function(
    ffi.Double,
    ffi.Double,
    ffi.Double,
    ffi.Double,
    ffi.Double,
    ffi.Double,
    ffi.Double,
    ffi.Double,
    ffi.Double,
    ffi.Double);
typedef DoubleDecenaryOp = double Function(double, double, double, double,
    double, double, double, double, double, double);

DoubleDecenaryOp sumManyDoubles = ffiTestFunctions
    .lookupFunction<NativeDoubleDecenaryOp, DoubleDecenaryOp>("SumManyDoubles");

void testNativeFunctionManyArguments2() {
  Expect.approxEquals(
      55.0, sumManyDoubles(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0));
}

typedef NativeVigesimalOp = ffi.Double Function(
    ffi.IntPtr,
    ffi.Float,
    ffi.IntPtr,
    ffi.Double,
    ffi.IntPtr,
    ffi.Float,
    ffi.IntPtr,
    ffi.Double,
    ffi.IntPtr,
    ffi.Float,
    ffi.IntPtr,
    ffi.Double,
    ffi.IntPtr,
    ffi.Float,
    ffi.IntPtr,
    ffi.Double,
    ffi.IntPtr,
    ffi.Float,
    ffi.IntPtr,
    ffi.Double);
typedef VigesimalOp = double Function(
    int,
    double,
    int,
    double,
    int,
    double,
    int,
    double,
    int,
    double,
    int,
    double,
    int,
    double,
    int,
    double,
    int,
    double,
    int,
    double);

VigesimalOp sumManyNumbers = ffiTestFunctions
    .lookupFunction<NativeVigesimalOp, VigesimalOp>("SumManyNumbers");

void testNativeFunctionManyArguments3() {
  Expect.approxEquals(
      210.0,
      sumManyNumbers(1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9, 10.0, 11, 12.0, 13,
          14.0, 15, 16.0, 17, 18.0, 19, 20.0));
}

typedef Int64PointerUnOp = ffi.Pointer<ffi.Int64> Function(
    ffi.Pointer<ffi.Int64>);

Int64PointerUnOp assign1337Index1 = ffiTestFunctions
    .lookupFunction<Int64PointerUnOp, Int64PointerUnOp>("Assign1337Index1");

void testNativeFunctionPointer() {
  ffi.Pointer<ffi.Int64> p2 = Pointer.allocate(count: 2);
  p2.store(42);
  p2.elementAt(1).store(1000);
  ffi.Pointer<ffi.Int64> result = assign1337Index1(p2);
  Expect.equals(1337, result.load<int>());
  Expect.equals(1337, p2.elementAt(1).load<int>());
  Expect.equals(p2.elementAt(1).address, result.address);
  p2.free();
}

void testNullInt() {
  BinaryOp sumPlus42 =
      ffiTestFunctions.lookupFunction<NativeBinaryOp, BinaryOp>("SumPlus42");

  Expect.throws(() => sumPlus42(43, null));
}

void testNullDouble() {
  Expect.throws(() => times1_337Double(null));
}

void testNullManyArgs() {
  Expect.throws(() => sumManyNumbers(1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9, 10.0,
      11, 12.0, 13, 14.0, 15, 16.0, 17, 18.0, null, 20.0));
}

Int64PointerUnOp nullableInt64ElemAt1 = ffiTestFunctions
    .lookupFunction<Int64PointerUnOp, Int64PointerUnOp>("NullableInt64ElemAt1");

void testNullPointers() {
  Pointer<ffi.Int64> result = nullableInt64ElemAt1(ffi.nullptr.cast());
  Expect.equals(result, ffi.nullptr);

  Pointer<ffi.Int64> p2 = Pointer.allocate(count: 2);
  result = nullableInt64ElemAt1(p2);
  Expect.notEquals(result, ffi.nullptr);
  p2.free();
}

typedef NativeFloatPointerToBool = ffi.Uint8 Function(ffi.Pointer<ffi.Float>);
typedef FloatPointerToBool = int Function(ffi.Pointer<ffi.Float>);

FloatPointerToBool isRoughly1337 = ffiTestFunctions.lookupFunction<
    NativeFloatPointerToBool, FloatPointerToBool>("IsRoughly1337");

void testFloatRounding() {
  Pointer<ffi.Float> p2 = Pointer.allocate();
  p2.store(1337.0);

  int result = isRoughly1337(p2);
  Expect.equals(1, result);

  p2.free();
}

typedef NativeFloatToVoid = ffi.Void Function(ffi.Float);
typedef DoubleToVoid = void Function(double);

DoubleToVoid devNullFloat = ffiTestFunctions
    .lookupFunction<NativeFloatToVoid, DoubleToVoid>("DevNullFloat");

void testVoidReturn() {
  devNullFloat(1337.0);

  dynamic loseSignature = devNullFloat;
  dynamic result = loseSignature(1337.0);
  Expect.isNull(result);
}

typedef NativeVoidToFloat = ffi.Float Function();
typedef VoidToDouble = double Function();

VoidToDouble inventFloatValue = ffiTestFunctions
    .lookupFunction<NativeVoidToFloat, VoidToDouble>("InventFloatValue");

void testNoArgs() {
  double result = inventFloatValue();
  Expect.approxEquals(1337.0, result);
}

// Throw an exception from within the trampoline and collect a stacktrace
// include its frame.
void testException() {
  try {
    sumPlus42(null, null);
  } catch (e, s) {
    print("$e, $s");
    return;
  }
  throw "Didn't throw!";
}
