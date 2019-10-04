// Copyright (c) 2019, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

library vm.bytecode.generics;

import 'dart:math' show min;

import 'package:kernel/ast.dart' hide MapEntry;
import 'package:kernel/core_types.dart' show CoreTypes;
import 'package:kernel/type_algebra.dart' show Substitution;
import 'package:kernel/type_environment.dart' show TypeEnvironment;

import 'options.dart' show BytecodeOptions;

bool hasInstantiatorTypeArguments(Class c) {
  for (; c != null; c = c.superclass) {
    if (c.typeParameters.isNotEmpty) {
      return true;
    }
  }
  return false;
}

List<DartType> getTypeParameterTypes(List<TypeParameter> typeParameters) {
  if (typeParameters.isEmpty) {
    return const <DartType>[];
  }
  final types = new List<DartType>(typeParameters.length);
  for (int i = 0; i < typeParameters.length; ++i) {
    types[i] = new TypeParameterType(typeParameters[i]);
  }
  return types;
}

bool _canReuseSuperclassTypeArguments(List<DartType> superTypeArgs,
    List<TypeParameter> typeParameters, int overlap) {
  for (int i = 0; i < overlap; ++i) {
    final superTypeArg = superTypeArgs[superTypeArgs.length - overlap + i];
    if (!(superTypeArg is TypeParameterType &&
        superTypeArg.parameter == typeParameters[i])) {
      return false;
    }
  }
  return true;
}

List<DartType> flattenInstantiatorTypeArguments(
    Class instantiatedClass, List<DartType> typeArgs) {
  final typeParameters = instantiatedClass.typeParameters;
  assert(typeArgs.length == typeParameters.length);

  final supertype = instantiatedClass.supertype;
  if (supertype == null) {
    return typeArgs;
  }

  final superTypeArgs = flattenInstantiatorTypeArguments(
      supertype.classNode, supertype.typeArguments);

  // Shrink type arguments by reusing portion of superclass type arguments
  // if there is an overlapping. This optimization should be consistent with
  // VM in order to correctly reuse instantiator type arguments.
  int overlap = min(superTypeArgs.length, typeArgs.length);
  for (; overlap > 0; --overlap) {
    if (_canReuseSuperclassTypeArguments(
        superTypeArgs, typeParameters, overlap)) {
      break;
    }
  }

  final substitution = Substitution.fromPairs(typeParameters, typeArgs);

  List<DartType> flatTypeArgs = <DartType>[];
  for (var type in superTypeArgs) {
    flatTypeArgs.add(substitution.substituteType(type));
  }
  flatTypeArgs.addAll(typeArgs.getRange(overlap, typeArgs.length));

  return flatTypeArgs;
}

List<DartType> getInstantiatorTypeArguments(
    Class instantiatedClass, List<DartType> typeArgs) {
  final flatTypeArgs =
      flattenInstantiatorTypeArguments(instantiatedClass, typeArgs);
  if (isAllDynamic(flatTypeArgs)) {
    return null;
  }
  return flatTypeArgs;
}

List<DartType> getDefaultFunctionTypeArguments(FunctionNode function) {
  final typeParameters = function.typeParameters;
  if (typeParameters.isEmpty) {
    return null;
  }
  bool dynamicOnly = true;
  for (var tp in typeParameters) {
    if (tp.defaultType != null && tp.defaultType != const DynamicType()) {
      dynamicOnly = false;
      break;
    }
  }
  if (dynamicOnly) {
    return null;
  }
  List<DartType> defaultTypes = <DartType>[];
  for (var tp in typeParameters) {
    defaultTypes.add(tp.defaultType ?? const DynamicType());
  }
  return defaultTypes;
}

bool isAllDynamic(List<DartType> typeArgs) {
  for (var t in typeArgs) {
    if (t != const DynamicType()) {
      return false;
    }
  }
  return true;
}

bool isInstantiatedGenericType(DartType type) =>
    (type is InterfaceType) &&
    type.typeArguments.isNotEmpty &&
    !hasFreeTypeParameters(type.typeArguments);

bool hasFreeTypeParameters(List<DartType> typeArgs) {
  final findTypeParams = new FindFreeTypeParametersVisitor();
  return typeArgs.any((t) => t.accept(findTypeParams));
}

class FindFreeTypeParametersVisitor extends DartTypeVisitor<bool> {
  Set<TypeParameter> _declaredTypeParameters;

  bool visit(DartType type) => type.accept(this);

  @override
  bool defaultDartType(DartType node) =>
      throw 'Unexpected type ${node.runtimeType} $node';

  @override
  bool visitInvalidType(InvalidType node) => false;

  @override
  bool visitDynamicType(DynamicType node) => false;

  @override
  bool visitVoidType(VoidType node) => false;

  @override
  bool visitBottomType(BottomType node) => false;

  @override
  bool visitTypeParameterType(TypeParameterType node) =>
      _declaredTypeParameters == null ||
      !_declaredTypeParameters.contains(node.parameter);

  @override
  bool visitInterfaceType(InterfaceType node) =>
      node.typeArguments.any((t) => t.accept(this));

  @override
  bool visitTypedefType(TypedefType node) =>
      node.typeArguments.any((t) => t.accept(this));

  @override
  bool visitFunctionType(FunctionType node) {
    if (node.typeParameters.isNotEmpty) {
      _declaredTypeParameters ??= new Set<TypeParameter>();
      _declaredTypeParameters.addAll(node.typeParameters);
    }

    final bool result = node.positionalParameters.any((t) => t.accept(this)) ||
        node.namedParameters.any((p) => p.type.accept(this)) ||
        node.returnType.accept(this);

    if (node.typeParameters.isNotEmpty) {
      _declaredTypeParameters.removeAll(node.typeParameters);
    }

    return result;
  }
}

/// Returns static type of [expr].
DartType getStaticType(Expression expr, TypeEnvironment typeEnvironment) {
  // TODO(dartbug.com/34496): Remove this try/catch once
  // getStaticType() is reliable.
  try {
    return expr.getStaticType(typeEnvironment);
  } catch (e) {
    return const DynamicType();
  }
}

/// Returns `true` if [type] cannot be extended in user code.
bool isSealedType(DartType type, CoreTypes coreTypes) {
  if (type is InterfaceType) {
    final cls = type.classNode;
    return cls == coreTypes.intClass ||
        cls == coreTypes.doubleClass ||
        cls == coreTypes.boolClass ||
        cls == coreTypes.stringClass ||
        cls == coreTypes.nullClass;
  }
  return false;
}

/// Returns true if an instance call to [interfaceTarget] with given
/// [receiver] can omit argument type checks needed due to generic-covariant
/// parameters.
bool isUncheckedCall(Member interfaceTarget, Expression receiver,
    TypeEnvironment typeEnvironment) {
  if (interfaceTarget == null) {
    // Dynamic call cannot be unchecked.
    return false;
  }

  if (!_hasGenericCovariantParameters(interfaceTarget)) {
    // Unchecked call makes sense only if there are generic-covariant parameters.
    return false;
  }

  // Calls via [this] do not require checks.
  if (receiver is ThisExpression) {
    return true;
  }

  DartType receiverStaticType = getStaticType(receiver, typeEnvironment);
  if (receiverStaticType is InterfaceType) {
    if (receiverStaticType.typeArguments.isEmpty) {
      return true;
    }

    if (receiverStaticType.typeArguments
        .every((t) => isSealedType(t, typeEnvironment.coreTypes))) {
      return true;
    }
  }
  return false;
}

/// If receiver type at run time matches static type we can omit argument type
/// checks. This condition can be efficiently tested if static receiver type is
/// fully instantiated (e.g. doesn't have type parameters).
/// [isInstantiatedInterfaceCall] tests if an instance call to
/// [interfaceTarget] with given [staticReceiverType] may benefit from
/// this optimization.
bool isInstantiatedInterfaceCall(
    Member interfaceTarget, DartType staticReceiverType) {
  // Providing instantiated receiver type wouldn't help in case of a
  // dynamic call or call without any parameter type checks.
  if (interfaceTarget == null ||
      !_hasGenericCovariantParameters(interfaceTarget)) {
    return false;
  }
  return isInstantiatedGenericType(staticReceiverType);
}

bool _hasGenericCovariantParameters(Member target) {
  if (target is Field) {
    return target.isGenericCovariantImpl;
  } else if (target is Procedure) {
    for (var param in target.function.positionalParameters) {
      if (param.isGenericCovariantImpl) {
        return true;
      }
    }
    for (var param in target.function.namedParameters) {
      if (param.isGenericCovariantImpl) {
        return true;
      }
    }
    return false;
  } else {
    throw 'Unexpected instance call target ${target.runtimeType} $target';
  }
}

/// Returns true if invocation [node] is a closure call with statically known
/// function type. Such invocations can omit argument type checks.
bool isUncheckedClosureCall(MethodInvocation node,
        TypeEnvironment typeEnvironment, BytecodeOptions options) =>
    node.name.name == 'call' &&
    getStaticType(node.receiver, typeEnvironment) is FunctionType &&
    !options.avoidClosureCallInstructions;
