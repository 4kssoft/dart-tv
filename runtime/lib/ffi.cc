// Copyright (c) 2019, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "lib/ffi.h"

#include "include/dart_api.h"
#include "platform/globals.h"
#include "vm/bootstrap_natives.h"
#include "vm/class_finalizer.h"
#include "vm/class_id.h"
#include "vm/compiler/assembler/assembler.h"
#include "vm/compiler/ffi.h"
#include "vm/compiler/jit/compiler.h"
#include "vm/exceptions.h"
#include "vm/flags.h"
#include "vm/log.h"
#include "vm/native_arguments.h"
#include "vm/native_entry.h"
#include "vm/object.h"
#include "vm/object_store.h"
#include "vm/symbols.h"

namespace dart {

// The following functions are runtime checks on type arguments.
// Some checks are also performed in kernel transformation, these are asserts.
// Some checks are only performed at runtime to allow for generic code, these
// throw ArgumentExceptions.

static bool IsPointerType(const AbstractType& type) {
  return RawObject::IsFfiPointerClassId(type.type_class_id());
}

static void CheckSized(const AbstractType& type_arg) {
  const classid_t type_cid = type_arg.type_class_id();
  if (RawObject::IsFfiNativeTypeTypeClassId(type_cid) ||
      RawObject::IsFfiTypeVoidClassId(type_cid) ||
      RawObject::IsFfiTypeNativeFunctionClassId(type_cid)) {
    const String& error = String::Handle(String::NewFormatted(
        "%s does not have a predefined size (@unsized). "
        "Unsized NativeTypes do not support [sizeOf] because their size "
        "is unknown. "
        "Consequently, [allocate], [Pointer.load], [Pointer.store], and "
        "[Pointer.elementAt] are not available.",
        String::Handle(type_arg.UserVisibleName()).ToCString()));
    Exceptions::ThrowArgumentError(error);
  }
}

enum class FfiVariance { kCovariant = 0, kContravariant = 1 };

// Checks that a dart type correspond to a [NativeType].
// Because this is checked already in a kernel transformation, it does not throw
// an ArgumentException but a boolean which should be asserted.
//
// [Int8]                               -> [int]
// [Int16]                              -> [int]
// [Int32]                              -> [int]
// [Int64]                              -> [int]
// [Uint8]                              -> [int]
// [Uint16]                             -> [int]
// [Uint32]                             -> [int]
// [Uint64]                             -> [int]
// [IntPtr]                             -> [int]
// [Double]                             -> [double]
// [Float]                              -> [double]
// [Pointer]<T>                         -> [Pointer]<T>
// T extends [Struct]                   -> T
// [NativeFunction]<T1 Function(T2, T3) -> S1 Function(S2, S3)
//    where DartRepresentationOf(Tn) -> Sn
static bool DartAndCTypeCorrespond(const AbstractType& native_type,
                                   const AbstractType& dart_type,
                                   FfiVariance variance) {
  classid_t native_type_cid = native_type.type_class_id();
  if (RawObject::IsFfiTypeIntClassId(native_type_cid)) {
    return dart_type.IsSubtypeOf(AbstractType::Handle(Type::IntType()),
                                 Heap::kNew);
  }
  if (RawObject::IsFfiTypeDoubleClassId(native_type_cid)) {
    return dart_type.IsSubtypeOf(AbstractType::Handle(Type::Double()),
                                 Heap::kNew);
  }
  if (RawObject::IsFfiPointerClassId(native_type_cid)) {
    return (variance == FfiVariance::kCovariant &&
            dart_type.IsSubtypeOf(native_type, Heap::kNew)) ||
           (variance == FfiVariance::kContravariant &&
            native_type.IsSubtypeOf(dart_type, Heap::kNew)) ||
           dart_type.IsNullType();
  }
  if (RawObject::IsFfiTypeNativeFunctionClassId(native_type_cid)) {
    if (!dart_type.IsFunctionType()) {
      return false;
    }
    TypeArguments& nativefunction_type_args =
        TypeArguments::Handle(native_type.arguments());
    AbstractType& nativefunction_type_arg =
        AbstractType::Handle(nativefunction_type_args.TypeAt(0));
    if (!nativefunction_type_arg.IsFunctionType()) {
      return false;
    }
    Function& dart_function =
        Function::Handle((Type::Cast(dart_type)).signature());
    if (dart_function.NumTypeParameters() != 0 ||
        dart_function.HasOptionalPositionalParameters() ||
        dart_function.HasOptionalNamedParameters()) {
      return false;
    }
    Function& nativefunction_function =
        Function::Handle(((Type&)nativefunction_type_arg).signature());
    if (nativefunction_function.NumTypeParameters() != 0 ||
        nativefunction_function.HasOptionalPositionalParameters() ||
        nativefunction_function.HasOptionalNamedParameters()) {
      return false;
    }
    if (!(dart_function.NumParameters() ==
          nativefunction_function.NumParameters())) {
      return false;
    }
    if (!DartAndCTypeCorrespond(
            AbstractType::Handle(nativefunction_function.result_type()),
            AbstractType::Handle(dart_function.result_type()), variance)) {
      return false;
    }
    for (intptr_t i = 0; i < dart_function.NumParameters(); i++) {
      if (!DartAndCTypeCorrespond(
              AbstractType::Handle(nativefunction_function.ParameterTypeAt(i)),
              AbstractType::Handle(dart_function.ParameterTypeAt(i)),
              variance)) {
        return false;
      }
    }
  }
  return true;
}

static void CheckDartAndCTypeCorrespond(const AbstractType& native_type,
                                        const AbstractType& dart_type,
                                        FfiVariance variance) {
  if (!DartAndCTypeCorrespond(native_type, dart_type, variance)) {
    const String& error = String::Handle(String::NewFormatted(
        "Expected type '%s' to be different, it should be "
        "DartRepresentationOf('%s').",
        String::Handle(dart_type.UserVisibleName()).ToCString(),
        String::Handle(native_type.UserVisibleName()).ToCString()));
    Exceptions::ThrowArgumentError(error);
  }
}

// The following functions are runtime checks on arguments.

static const Pointer& AsPointer(const Instance& instance) {
  if (!instance.IsPointer()) {
    const String& error = String::Handle(String::NewFormatted(
        "Expected a Pointer object but found %s", instance.ToCString()));
    Exceptions::ThrowArgumentError(error);
  }
  return Pointer::Cast(instance);
}

static const Integer& AsInteger(const Instance& instance) {
  if (!instance.IsInteger()) {
    const String& error = String::Handle(String::NewFormatted(
        "Expected an int but found %s", instance.ToCString()));
    Exceptions::ThrowArgumentError(error);
  }
  return Integer::Cast(instance);
}

static const Double& AsDouble(const Instance& instance) {
  if (!instance.IsDouble()) {
    const String& error = String::Handle(String::NewFormatted(
        "Expected a double but found %s", instance.ToCString()));
    Exceptions::ThrowArgumentError(error);
  }
  return Double::Cast(instance);
}

// Calcuate the size of a native type.
//
// You must check [IsConcreteNativeType] and [CheckSized] first to verify that
// this type has a defined size.
static size_t SizeOf(const AbstractType& type) {
  if (RawObject::IsFfiTypeClassId(type.type_class_id())) {
    return compiler::ffi::ElementSizeInBytes(type.type_class_id());
  } else {
    Class& struct_class = Class::Handle(type.type_class());
    Object& result = Object::Handle(
        struct_class.InvokeGetter(Symbols::SizeOfStructField(),
                                  /*throw_nsm_if_absent=*/false,
                                  /*respect_reflectable=*/false));
    ASSERT(!result.IsNull() && result.IsInteger());
    return Integer::Cast(result).AsInt64Value();
  }
}

// The remainder of this file implements the dart:ffi native methods.

DEFINE_NATIVE_ENTRY(Ffi_allocate, 1, 1) {
  GET_NATIVE_TYPE_ARGUMENT(type_arg, arguments->NativeTypeArgAt(0));

  CheckSized(type_arg);
  size_t element_size = SizeOf(type_arg);

  GET_NON_NULL_NATIVE_ARGUMENT(Integer, argCount, arguments->NativeArgAt(0));
  int64_t count = argCount.AsInt64Value();
  size_t size = element_size * count;  // Truncates overflow.
  size_t memory = reinterpret_cast<size_t>(malloc(size));
  if (memory == 0) {
    const String& error = String::Handle(String::NewFormatted(
        "allocating (%" Pd ") bytes of memory failed", size));
    Exceptions::ThrowArgumentError(error);
  }

  RawPointer* result = Pointer::New(type_arg, memory);
  return result;
}

DEFINE_NATIVE_ENTRY(Ffi_fromAddress, 1, 1) {
  GET_NATIVE_TYPE_ARGUMENT(type_arg, arguments->NativeTypeArgAt(0));
  GET_NON_NULL_NATIVE_ARGUMENT(Integer, arg_ptr, arguments->NativeArgAt(0));
  return Pointer::New(type_arg, arg_ptr.AsInt64Value());
}

DEFINE_NATIVE_ENTRY(Ffi_elementAt, 0, 2) {
  GET_NON_NULL_NATIVE_ARGUMENT(Pointer, pointer, arguments->NativeArgAt(0));
  GET_NON_NULL_NATIVE_ARGUMENT(Integer, index, arguments->NativeArgAt(1));
  AbstractType& pointer_type_arg =
      AbstractType::Handle(zone, pointer.type_argument());
  CheckSized(pointer_type_arg);
  return Pointer::New(pointer_type_arg,
                      pointer.NativeAddress() +
                          index.AsInt64Value() * SizeOf(pointer_type_arg));
}

DEFINE_NATIVE_ENTRY(Ffi_offsetBy, 0, 2) {
  GET_NON_NULL_NATIVE_ARGUMENT(Pointer, pointer, arguments->NativeArgAt(0));
  GET_NON_NULL_NATIVE_ARGUMENT(Integer, offset, arguments->NativeArgAt(1));
  AbstractType& pointer_type_arg =
      AbstractType::Handle(pointer.type_argument());

  return Pointer::New(pointer_type_arg,
                      pointer.NativeAddress() + offset.AsInt64Value());
}

DEFINE_NATIVE_ENTRY(Ffi_cast, 1, 1) {
  GET_NON_NULL_NATIVE_ARGUMENT(Pointer, pointer, arguments->NativeArgAt(0));
  GET_NATIVE_TYPE_ARGUMENT(type_arg, arguments->NativeTypeArgAt(0));
  return Pointer::New(type_arg, pointer.NativeAddress());
}

DEFINE_NATIVE_ENTRY(Ffi_free, 0, 1) {
  GET_NON_NULL_NATIVE_ARGUMENT(Pointer, pointer, arguments->NativeArgAt(0));

  free(reinterpret_cast<void*>(pointer.NativeAddress()));
  pointer.SetNativeAddress(0);

  return Object::null();
}

DEFINE_NATIVE_ENTRY(Ffi_address, 0, 1) {
  GET_NON_NULL_NATIVE_ARGUMENT(Pointer, pointer, arguments->NativeArgAt(0));
  return Integer::New(pointer.NativeAddress());
}

static RawObject* LoadValue(Zone* zone,
                            const Pointer& target,
                            const AbstractType& instance_type_arg) {
  classid_t type_cid = instance_type_arg.type_class_id();
  size_t address = target.NativeAddress();
  switch (type_cid) {
    case kFfiInt8Cid:
      return Integer::New(*reinterpret_cast<int8_t*>(address));
    case kFfiInt16Cid:
      return Integer::New(*reinterpret_cast<int16_t*>(address));
    case kFfiInt32Cid:
      return Integer::New(*reinterpret_cast<int32_t*>(address));
    case kFfiInt64Cid:
      return Integer::New(*reinterpret_cast<int64_t*>(address));
    case kFfiUint8Cid:
      return Integer::NewFromUint64(*reinterpret_cast<uint8_t*>(address));
    case kFfiUint16Cid:
      return Integer::NewFromUint64(*reinterpret_cast<uint16_t*>(address));
    case kFfiUint32Cid:
      return Integer::NewFromUint64(*reinterpret_cast<uint32_t*>(address));
    case kFfiUint64Cid:
      return Integer::NewFromUint64(*reinterpret_cast<uint64_t*>(address));
    case kFfiIntPtrCid:
      return Integer::New(*reinterpret_cast<intptr_t*>(address));
    case kFfiFloatCid:
      return Double::New(*reinterpret_cast<float_t*>(address));
    case kFfiDoubleCid:
      return Double::New(*reinterpret_cast<double_t*>(address));
    default: {
      if (IsPointerType(instance_type_arg)) {
        const AbstractType& type_arg = AbstractType::Handle(
            TypeArguments::Handle(instance_type_arg.arguments())
                .TypeAt(Pointer::kNativeTypeArgPos));
        return Pointer::New(type_arg, reinterpret_cast<size_t>(
                                          *reinterpret_cast<void**>(address)));
      } else {
        // Result is a struct class -- find <class name>.#fromPointer
        // constructor and call it.
        Class& cls = Class::Handle(zone, instance_type_arg.type_class());
        const Function& constructor =
            Function::Handle(cls.LookupFunctionAllowPrivate(String::Handle(
                String::Concat(String::Handle(String::Concat(
                                   String::Handle(cls.Name()), Symbols::Dot())),
                               Symbols::StructFromPointer()))));
        ASSERT(!constructor.IsNull());
        ASSERT(constructor.IsGenerativeConstructor());
        ASSERT(!Object::Handle(constructor.VerifyCallEntryPoint()).IsError());
        Instance& new_object = Instance::Handle(Instance::New(cls));
        new_object.SetTypeArguments(
            TypeArguments::Handle(instance_type_arg.arguments()));
        ASSERT(cls.is_allocated() ||
               Dart::vm_snapshot_kind() != Snapshot::kFullAOT);
        const Array& args = Array::Handle(zone, Array::New(2));
        args.SetAt(0, new_object);
        args.SetAt(1, target);
        Object& constructorResult =
            Object::Handle(DartEntry::InvokeFunction(constructor, args));
        ASSERT(!constructorResult.IsError());
        return new_object.raw();
      }
    }
  }
}

DEFINE_NATIVE_ENTRY(Ffi_load, 1, 1) {
  GET_NON_NULL_NATIVE_ARGUMENT(Pointer, pointer, arguments->NativeArgAt(0));
  GET_NATIVE_TYPE_ARGUMENT(type_arg, arguments->NativeTypeArgAt(0));
  AbstractType& pointer_type_arg =
      AbstractType::Handle(pointer.type_argument());
  CheckSized(pointer_type_arg);
  CheckDartAndCTypeCorrespond(pointer_type_arg, type_arg,
                              FfiVariance::kContravariant);

  return LoadValue(zone, pointer, pointer_type_arg);
}

static void StoreValue(Zone* zone,
                       const Pointer& pointer,
                       classid_t type_cid,
                       const Instance& new_value) {
  uint8_t* const address = reinterpret_cast<uint8_t*>(pointer.NativeAddress());
  AbstractType& pointer_type_arg =
      AbstractType::Handle(pointer.type_argument());
  switch (type_cid) {
    case kFfiInt8Cid:
      *reinterpret_cast<int8_t*>(address) = AsInteger(new_value).AsInt64Value();
      break;
    case kFfiInt16Cid:
      *reinterpret_cast<int16_t*>(address) =
          AsInteger(new_value).AsInt64Value();
      break;
    case kFfiInt32Cid:
      *reinterpret_cast<int32_t*>(address) =
          AsInteger(new_value).AsInt64Value();
      break;
    case kFfiInt64Cid:
      *reinterpret_cast<int64_t*>(address) =
          AsInteger(new_value).AsInt64Value();
      break;
    case kFfiUint8Cid:
      *reinterpret_cast<uint8_t*>(address) =
          AsInteger(new_value).AsInt64Value();
      break;
    case kFfiUint16Cid:
      *reinterpret_cast<uint16_t*>(address) =
          AsInteger(new_value).AsInt64Value();
      break;
    case kFfiUint32Cid:
      *reinterpret_cast<uint32_t*>(address) =
          AsInteger(new_value).AsInt64Value();
      break;
    case kFfiUint64Cid:
      *reinterpret_cast<uint64_t*>(address) =
          AsInteger(new_value).AsInt64Value();
      break;
    case kFfiIntPtrCid:
      *reinterpret_cast<intptr_t*>(address) =
          AsInteger(new_value).AsInt64Value();
      break;
    case kFfiFloatCid:
      *reinterpret_cast<float*>(address) = AsDouble(new_value).value();
      break;
    case kFfiDoubleCid:
      *reinterpret_cast<double*>(address) = AsDouble(new_value).value();
      break;
    case kFfiPointerCid: {
      ASSERT(IsPointerType(pointer_type_arg));
      ASSERT(new_value.IsPointer());
      const void* const stored =
          reinterpret_cast<void*>(AsPointer(new_value).NativeAddress());
      *reinterpret_cast<const void**>(address) = stored;
      break;
    }
    default:
      UNREACHABLE();
  }
}

DEFINE_NATIVE_ENTRY(Ffi_store, 0, 2) {
  GET_NON_NULL_NATIVE_ARGUMENT(Pointer, pointer, arguments->NativeArgAt(0));
  GET_NATIVE_ARGUMENT(Instance, new_value, arguments->NativeArgAt(1));
  AbstractType& arg_type = AbstractType::Handle(new_value.GetType(Heap::kNew));
  AbstractType& pointer_type_arg =
      AbstractType::Handle(pointer.type_argument());
  CheckSized(pointer_type_arg);
  CheckDartAndCTypeCorrespond(pointer_type_arg, arg_type,
                              FfiVariance::kCovariant);

  if (new_value.IsNull()) {
    const String& error = String::Handle(
        String::NewFormatted("Argument to Pointer.store is null."));
    Exceptions::ThrowArgumentError(error);
  }

  classid_t type_cid = pointer_type_arg.type_class_id();
  StoreValue(zone, pointer, type_cid, new_value);
  return Object::null();
}

DEFINE_NATIVE_ENTRY(Ffi_sizeOf, 1, 0) {
  GET_NATIVE_TYPE_ARGUMENT(type_arg, arguments->NativeTypeArgAt(0));
  CheckSized(type_arg);

  return Integer::New(SizeOf(type_arg));
}


// Static invocations to this method are translated directly in streaming FGB
// and bytecode FGB. However, we can still reach this entrypoint in the bytecode
// interpreter.
DEFINE_NATIVE_ENTRY(Ffi_asFunctionInternal, 2, 1) {
#if defined(DART_PRECOMPILED_RUNTIME) || defined(DART_PRECOMPILER)
  UNREACHABLE();
#else
  ASSERT(FLAG_enable_interpreter);

  GET_NON_NULL_NATIVE_ARGUMENT(Pointer, pointer, arguments->NativeArgAt(0));
  GET_NATIVE_TYPE_ARGUMENT(dart_type, arguments->NativeTypeArgAt(0));
  GET_NATIVE_TYPE_ARGUMENT(native_type, arguments->NativeTypeArgAt(1));

  const Function& dart_signature =
      Function::Handle(zone, Type::Cast(dart_type).signature());
  const Function& native_signature =
      Function::Handle(zone, Type::Cast(native_type).signature());
  const Function& function = Function::Handle(
      compiler::ffi::TrampolineFunction(dart_signature, native_signature));

  // Set the c function pointer in the context of the closure rather than in
  // the function so that we can reuse the function for each c function with
  // the same signature.
  const Context& context = Context::Handle(Context::New(1));
  context.SetAt(0,
                Integer::Handle(zone, Integer::New(pointer.NativeAddress())));

  return Closure::New(Object::null_type_arguments(),
                      Object::null_type_arguments(), function, context,
                      Heap::kOld);
#endif
}

DEFINE_NATIVE_ENTRY(Ffi_asExternalTypedData, 0, 2) {
  GET_NON_NULL_NATIVE_ARGUMENT(Pointer, pointer, arguments->NativeArgAt(0));
  GET_NON_NULL_NATIVE_ARGUMENT(Integer, count, arguments->NativeArgAt(1));
  const auto& pointer_type_arg = AbstractType::Handle(pointer.type_argument());
  const classid_t type_cid = pointer_type_arg.type_class_id();
  classid_t cid = 0;

  switch (type_cid) {
    case kFfiInt8Cid:
      cid = kExternalTypedDataInt8ArrayCid;
      break;
    case kFfiUint8Cid:
      cid = kExternalTypedDataUint8ArrayCid;
      break;
    case kFfiInt16Cid:
      cid = kExternalTypedDataInt16ArrayCid;
      break;
    case kFfiUint16Cid:
      cid = kExternalTypedDataUint16ArrayCid;
      break;
    case kFfiInt32Cid:
      cid = kExternalTypedDataInt32ArrayCid;
      break;
    case kFfiUint32Cid:
      cid = kExternalTypedDataUint32ArrayCid;
      break;
    case kFfiInt64Cid:
      cid = kExternalTypedDataInt64ArrayCid;
      break;
    case kFfiUint64Cid:
      cid = kExternalTypedDataUint64ArrayCid;
      break;
    case kFfiIntPtrCid:
      cid = kWordSize == 4 ? kExternalTypedDataInt32ArrayCid
                           : kExternalTypedDataInt64ArrayCid;
      break;
    case kFfiFloatCid:
      cid = kExternalTypedDataFloat32ArrayCid;
      break;
    case kFfiDoubleCid:
      cid = kExternalTypedDataFloat64ArrayCid;
      break;
    default: {
      const String& error = String::Handle(
          String::NewFormatted("Cannot create a TypedData from a Pointer to %s",
                               pointer_type_arg.ToCString()));
      Exceptions::ThrowArgumentError(error);
      UNREACHABLE();
    }
  }

  const intptr_t element_count = count.AsInt64Value();

  if (element_count < 0 ||
      element_count > ExternalTypedData::MaxElements(cid)) {
    const String& error = String::Handle(
        String::NewFormatted("Count must be in the range [0, %" Pd "].",
                             ExternalTypedData::MaxElements(cid)));
    Exceptions::ThrowArgumentError(error);
  }

  // The address must be aligned by the element size.
  const intptr_t element_size = ExternalTypedData::ElementSizeFor(cid);
  if (!Utils::IsAligned(pointer.NativeAddress(), element_size)) {
    const String& error = String::Handle(
        String::NewFormatted("Pointer address must be aligned to a multiple of"
                             "the element size (%" Pd ").",
                             element_size));
    Exceptions::ThrowArgumentError(error);
  }

  const auto& typed_data_class =
      Class::Handle(zone, isolate->class_table()->At(cid));
  const auto& error =
      Error::Handle(zone, typed_data_class.EnsureIsFinalized(thread));
  if (!error.IsNull()) {
    Exceptions::PropagateError(error);
  }

  return ExternalTypedData::New(
      cid, reinterpret_cast<uint8_t*>(pointer.NativeAddress()), element_count,
      Heap::kNew);
}

DEFINE_NATIVE_ENTRY(Ffi_nativeCallbackFunction, 1, 2) {
#if defined(TARGET_ARCH_DBC)
  Exceptions::ThrowUnsupportedError(
      "FFI callbacks are not yet supported on DBC.");
#elif defined(DART_PRECOMPILED_RUNTIME) || defined(DART_PRECOMPILER)
  // Calls to this function are removed by the flow-graph builder in AOT.
  // See StreamingFlowGraphBuilder::BuildFfiNativeCallbackFunction().
  UNREACHABLE();
#else
  GET_NATIVE_TYPE_ARGUMENT(type_arg, arguments->NativeTypeArgAt(0));
  GET_NON_NULL_NATIVE_ARGUMENT(Closure, closure, arguments->NativeArgAt(0));
  GET_NON_NULL_NATIVE_ARGUMENT(Instance, exceptional_return,
                               arguments->NativeArgAt(1));

  ASSERT(type_arg.IsInstantiated() && type_arg.IsFunctionType());
  const Function& native_signature =
      Function::Handle(zone, Type::Cast(type_arg).signature());
  Function& func = Function::Handle(zone, closure.function());

  // The FE verifies that the target of a 'fromFunction' is a static method, so
  // the value we see here must be a static tearoff. See ffi_use_sites.dart for
  // details.
  //
  // TODO(36748): Define hot-reload semantics of native callbacks. We may need
  // to look up the target by name.
  ASSERT(func.IsImplicitClosureFunction());
  func = func.parent_function();
  ASSERT(func.is_static());

  // We are returning an object which is not an Instance here. This is only OK
  // because we know that the result will be passed directly to
  // _pointerFromFunction and will not leak out into user code.
  arguments->SetReturn(
      Function::Handle(zone, compiler::ffi::NativeCallbackFunction(
                                 native_signature, func, exceptional_return)));

  // Because we have already set the return value.
  return Object::sentinel().raw();
#endif
}

DEFINE_NATIVE_ENTRY(Ffi_pointerFromFunction, 1, 1) {
  GET_NATIVE_TYPE_ARGUMENT(type_arg, arguments->NativeTypeArgAt(0));
  const Function& function =
      Function::CheckedHandle(zone, arguments->NativeArg0());

  Code& code = Code::Handle(zone);

#if defined(DART_PRECOMPILED_RUNTIME)
  code = function.CurrentCode();

  // Blobs snapshots don't support BSS-relative relocations required by native
  // callbacks (yet). Issue an error if the code has an unpatched relocation.
  if (!code.VerifyBSSRelocations()) {
    Exceptions::ThrowUnsupportedError(
        "FFI callbacks are not yet supported in blobs snapshots. Please use "
        "ELF or Assembly snapshots instead.");
  }
#else
  // We compile the callback immediately because we need to return a pointer to
  // the entry-point. Native calls do not use patching like Dart calls, so we
  // cannot compile it lazily.
  const Object& result = Object::Handle(
      zone, Compiler::CompileOptimizedFunction(thread, function));
  if (result.IsError()) {
    Exceptions::PropagateError(Error::Cast(result));
  }
  ASSERT(result.IsCode());
  code ^= result.raw();
#endif

  ASSERT(!code.IsNull());
  thread->SetFfiCallbackCode(function.FfiCallbackId(), code);

  uword entry_point = code.EntryPoint();
#if !defined(DART_PRECOMPILED_RUNTIME) && !defined(TARGET_ARCH_DBC)
  if (NativeCallbackTrampolines::Enabled()) {
    entry_point = isolate->native_callback_trampolines()->TrampolineForId(
        function.FfiCallbackId());
  }
#endif

  return Pointer::New(type_arg, entry_point);
}

#if defined(TARGET_ARCH_DBC)

void FfiMarshalledArguments::SetFunctionAddress(uint64_t value) const {
  data_[kOffsetFunctionAddress] = value;
}

static intptr_t ArgumentHostRegisterIndex(host::Register reg) {
  for (intptr_t i = 0; i < host::CallingConventions::kNumArgRegs; i++) {
    if (host::CallingConventions::ArgumentRegisters[i] == reg) {
      return i;
    }
  }
  UNREACHABLE();
}

void FfiMarshalledArguments::SetRegister(host::Register reg,
                                         uint64_t value) const {
  const intptr_t reg_index = ArgumentHostRegisterIndex(reg);
  ASSERT(host::CallingConventions::ArgumentRegisters[reg_index] == reg);
  const intptr_t index = kOffsetRegisters + reg_index;
  data_[index] = value;
}

void FfiMarshalledArguments::SetFpuRegister(host::FpuRegister reg,
                                            uint64_t value) const {
  const intptr_t fpu_index = static_cast<intptr_t>(reg);
  ASSERT(host::CallingConventions::FpuArgumentRegisters[fpu_index] == reg);
  const intptr_t index = kOffsetFpuRegisters + fpu_index;
  data_[index] = value;
}

void FfiMarshalledArguments::SetNumStackSlots(intptr_t num_args) const {
  data_[kOffsetNumStackSlots] = num_args;
}

void FfiMarshalledArguments::SetAlignmentMask(uint64_t alignment_mask) const {
  data_[kOffsetAlignmentMask] = alignment_mask;
}

intptr_t FfiMarshalledArguments::GetNumStackSlots() const {
  return data_[kOffsetNumStackSlots];
}

void FfiMarshalledArguments::SetStackSlotValue(intptr_t index,
                                               uint64_t value) const {
  ASSERT(0 <= index && index < GetNumStackSlots());
  data_[kOffsetStackSlotValues + index] = value;
}

uint64_t* FfiMarshalledArguments::New(
    const compiler::ffi::FfiSignatureDescriptor& signature,
    const uint64_t* arg_values) {
  const intptr_t num_stack_slots = signature.num_stack_slots();
  const uint64_t alignment_mask = ~(OS::ActivationFrameAlignment() - 1);
  const intptr_t size =
      FfiMarshalledArguments::kOffsetStackSlotValues + num_stack_slots;
  uint64_t* data = Thread::Current()->GetFfiMarshalledArguments(size);
  const auto& descr = FfiMarshalledArguments(data);

  descr.SetFunctionAddress(arg_values[compiler::ffi::kFunctionAddressRegister]);
  const intptr_t num_args = signature.length();
  descr.SetNumStackSlots(num_stack_slots);
  descr.SetAlignmentMask(alignment_mask);
  for (int i = 0; i < num_args; i++) {
    uint64_t arg_value = arg_values[compiler::ffi::kFirstArgumentRegister + i];
    HostLocation loc = signature.LocationAt(i);
    // TODO(36809): For 32 bit, support pair locations.
    if (loc.IsRegister()) {
      descr.SetRegister(loc.reg(), arg_value);
    } else if (loc.IsFpuRegister()) {
      descr.SetFpuRegister(loc.fpu_reg(), arg_value);
    } else {
      ASSERT(loc.IsStackSlot() || loc.IsDoubleStackSlot());
      ASSERT(loc.stack_index() < num_stack_slots);
      descr.SetStackSlotValue(loc.stack_index(), arg_value);
    }
  }

  return data;
}

#if defined(DEBUG)
void FfiMarshalledArguments::Print() const {
  OS::PrintErr("FfiMarshalledArguments data_ 0x%" Pp "\n",
               reinterpret_cast<intptr_t>(data_));
  OS::PrintErr("  00 0x%016" Px64 " (function address, int result)\n",
               data_[0]);
  for (intptr_t i = 0; i < host::CallingConventions::kNumArgRegs; i++) {
    const intptr_t index = kOffsetRegisters + i;
    const char* result_str = i == 0 ? ", float result" : "";
    OS::PrintErr("  %02" Pd " 0x%016" Px64 " (%s%s)\n", index, data_[index],
                 RegisterNames::RegisterName(
                     host::CallingConventions::ArgumentRegisters[i]),
                 result_str);
  }
  for (intptr_t i = 0; i < host::CallingConventions::kNumFpuArgRegs; i++) {
    const intptr_t index = kOffsetFpuRegisters + i;
    OS::PrintErr("  %02" Pd " 0x%016" Px64 " (%s)\n", index, data_[index],
                 RegisterNames::FpuRegisterName(
                     host::CallingConventions::FpuArgumentRegisters[i]));
  }
  const intptr_t alignment_mask = data_[kOffsetAlignmentMask];
  OS::PrintErr("  %02" Pd " 0x%" Pp " (stack alignment mask)\n",
               kOffsetAlignmentMask, alignment_mask);
  const intptr_t num_stack_slots = data_[kOffsetNumStackSlots];
  OS::PrintErr("  %02" Pd " 0x%" Pp " (number of stack slots)\n",
               kOffsetNumStackSlots, num_stack_slots);
  for (intptr_t i = 0; i < num_stack_slots; i++) {
    const intptr_t index = kOffsetStackSlotValues + i;
    OS::PrintErr("  %02" Pd " 0x%016" Px64 " (stack slot %" Pd ")\n", index,
                 data_[index], i);
  }
}
#endif  // defined(DEBUG)

#endif  // defined(TARGET_ARCH_DBC)

}  // namespace dart
