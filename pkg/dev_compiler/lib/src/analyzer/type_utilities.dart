// Copyright (c) 2015, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'dart:collection';

import 'package:analyzer/dart/element/element.dart';
import 'package:analyzer/dart/element/nullability_suffix.dart';
import 'package:analyzer/dart/element/type.dart';
import 'package:analyzer/src/dart/element/element.dart';
import 'package:analyzer/src/dart/element/member.dart' show TypeParameterMember;

import '../analyzer/element_helpers.dart';
import '../compiler/js_names.dart' as js_ast;
import '../js_ast/js_ast.dart' as js_ast;
import '../js_ast/js_ast.dart' show js;

/// Return the [InterfaceType] that itself has the legacy nullability, and for
/// every type parameter a [TypeParameterType] instance with the legacy
/// nullability is used as the corresponding type argument.
InterfaceType getLegacyRawClassType(ClassElement element) {
  var typeParameters = element.typeParameters;
  var typeArguments = typeParameters.map(getLegacyTypeParameterType).toList();
  return element.instantiate(
    typeArguments: typeArguments,
    nullabilitySuffix: NullabilitySuffix.star,
  );
}

/// Return the [TypeParameterType] with the legacy nullability for the given
/// type parameter [element].
TypeParameterType getLegacyTypeParameterType(TypeParameterElement element) {
  return element.instantiate(nullabilitySuffix: NullabilitySuffix.star);
}

/// Return the raw type (i.e. the type where type parameters are replaced with
/// the corresponding [TypeParameterType]) for the given [element]. The type
/// returned, and every [TypeParameterType] instance will have the legacy
/// nullability suffix.
DartType getLegacyElementType(TypeDefiningElement element) {
  if (element is ClassElement) {
    return getLegacyRawClassType(element);
  } else if (element is DynamicElementImpl) {
    return element.type;
  } else if (element is TypeParameterElement) {
    return getLegacyTypeParameterType(element);
  } else {
    throw StateError('Unsupported element: (${element.runtimeType}) $element');
  }
}

Set<TypeParameterElement> freeTypeParameters(DartType t) {
  var result = Set<TypeParameterElement>();
  void find(DartType t) {
    if (t is TypeParameterType) {
      result.add(t.element);
    } else if (t is FunctionType) {
      if (t.name != '' && t.name != null) {
        // For a typedef type like `Foo<T>`, we only need to check if the
        // type argument `T` has or is a free type parameter.
        //
        // For example, if `Foo` is a typedef type and `S` is a type parameter,
        // then the type `Foo<List<S>>` has `S` as its free type parameter,
        // regardless of what function is declared by the typedef.
        //
        // Also we need to find free type parameters whether or not they're
        // actually used by the typedef's function type (for example,
        // `typedef Foo<T> = int Function()` does not use `T`). So we must visit
        // the type arguments, instead of the substituted parameter and return
        // types.
        t.typeArguments.forEach(find);
      } else {
        find(t.returnType);
        t.parameters.forEach((p) => find(p.type));
        t.typeFormals.forEach((p) => find(p.bound));
        t.typeFormals.forEach(result.remove);
      }
    } else if (t is InterfaceType) {
      t.typeArguments.forEach(find);
    }
  }

  find(t);
  return result;
}

/// _CacheTable tracks cache variables for variables that
/// are emitted in place with a hoisted variable for a cache.
class _CacheTable {
  /// Mapping from types to their canonical names.
  // Use a LinkedHashMap to maintain key insertion order so the generated code
  // is stable under slight perturbation.  (If this is not good enough we could
  // sort by name to canonicalize order.)
  final _names = LinkedHashMap<DartType, js_ast.TemporaryId>(
      equals: typesAreEqual, hashCode: typeHashCode);
  Iterable<DartType> get keys => _names.keys.toList();

  js_ast.Statement _dischargeType(DartType type) {
    var name = _names.remove(type);
    if (name != null) {
      return js.statement('let #;', [name]);
    }
    return null;
  }

  /// Emit a list of statements declaring the cache variables for
  /// types tracked by this table.  If [typeFilter] is given,
  /// only emit the types listed in the filter.
  List<js_ast.Statement> discharge([Iterable<DartType> typeFilter]) {
    var decls = <js_ast.Statement>[];
    var types = typeFilter ?? keys;
    for (var t in types) {
      var stmt = _dischargeType(t);
      if (stmt != null) decls.add(stmt);
    }
    return decls;
  }

  bool isNamed(DartType type) => _names.containsKey(type);

  String _safeTypeName(String name) {
    if (name == "<bottom>") return "bottom";
    return name;
  }

  String _typeString(DartType type, {bool flat = false}) {
    if (type is ParameterizedType && type.name != null) {
      var clazz = type.name;
      var params = type.typeArguments;
      if (params == null) return clazz;
      if (params.every((p) => p.isDynamic)) return clazz;
      var paramStrings = params.map(_typeString);
      var paramString = paramStrings.join("\$");
      return "${clazz}Of${paramString}";
    }
    if (type is FunctionType) {
      if (flat) return "Fn";
      var rType = _typeString(type.returnType, flat: true);
      var paramStrings = type.normalParameterTypes
          .take(3)
          .map((p) => _typeString(p, flat: true));
      var paramString = paramStrings.join("And");
      var count = type.normalParameterTypes.length;
      if (count > 3 ||
          type.namedParameterTypes.isNotEmpty ||
          type.optionalParameterTypes.isNotEmpty) {
        paramString = "${paramString}__";
      } else if (count == 0) {
        paramString = "Void";
      }
      return "${paramString}To${rType}";
    }
    if (type is TypeParameterType) return type.name;
    return _safeTypeName(type.name ?? "type");
  }

  /// Heuristically choose a good name for the cache and generator
  /// variables.
  js_ast.TemporaryId chooseTypeName(DartType type) {
    return js_ast.TemporaryId(_typeString(type));
  }
}

/// _GeneratorTable tracks types which have been
/// named and hoisted.
class _GeneratorTable extends _CacheTable {
  final _defs = LinkedHashMap<DartType, js_ast.Expression>(
      equals: typesAreEqual, hashCode: typeHashCode);

  final js_ast.Identifier _runtimeModule;

  _GeneratorTable(this._runtimeModule);

  @override
  js_ast.Statement _dischargeType(DartType t) {
    var name = _names.remove(t);
    if (name != null) {
      js_ast.Expression init = _defs.remove(t);
      assert(init != null);
      return js.statement('let # = () => ((# = #.constFn(#))());',
          [name, name, _runtimeModule, init]);
    }
    return null;
  }

  /// If [type] does not already have a generator name chosen for it,
  /// assign it one, using [typeRep] as the initializer for it.
  /// Emit the generator name.
  js_ast.TemporaryId _nameType(DartType type, js_ast.Expression typeRep) {
    var temp = _names[type];
    if (temp == null) {
      _names[type] = temp = chooseTypeName(type);
      _defs[type] = typeRep;
    }
    return temp;
  }
}

class TypeTable {
  /// Generator variable names for hoisted types.
  final _GeneratorTable _generators;

  /// Mapping from type parameters to the types which must have their
  /// cache/generator variables discharged at the binding site for the
  /// type variable since the type definition depends on the type
  /// parameter.
  final _scopeDependencies = <TypeParameterElement, List<DartType>>{};

  TypeTable(js_ast.Identifier runtime) : _generators = _GeneratorTable(runtime);

  /// Emit a list of statements declaring the cache variables and generator
  /// definitions tracked by the table.  If [formals] is present, only
  /// emit the definitions which depend on the formals.
  List<js_ast.Statement> discharge([List<TypeParameterElement> formals]) {
    var filter = formals?.expand((p) => _scopeDependencies[p] ?? <DartType>[]);
    var stmts = [_generators].expand((c) => c.discharge(filter)).toList();
    formals?.forEach(_scopeDependencies.remove);
    return stmts;
  }

  /// Record the dependencies of the type on its free variables
  bool recordScopeDependencies(DartType type) {
    var freeVariables = freeTypeParameters(type);
    // TODO(leafp): This is a hack to avoid trying to hoist out of
    // generic functions and generic function types.  This often degrades
    // readability to little or no benefit.  It would be good to do this
    // when we know that we can hoist it to an outer scope, but for
    // now we just disable it.
    if (freeVariables.any((i) =>
        i.enclosingElement is FunctionTypedElement ||
        // Strict function types don't have element, so their type parameters
        // don't have any enclosing element. Analyzer started returning
        // strict function types for generic methods.
        i.enclosingElement == null)) {
      return true;
    }

    for (var free in freeVariables) {
      // If `free` is a promoted type parameter, get the original one so we can
      // find it in our map.
      var key = free is TypeParameterMember ? free.baseElement : free;
      _scopeDependencies.putIfAbsent(key, () => []).add(type);
    }
    return false;
  }

  /// Given a type [type], and a JS expression [typeRep] which implements it,
  /// add the type and its representation to the table, returning an
  /// expression which implements the type (but which caches the value).
  js_ast.Expression nameType(
      ParameterizedType type, js_ast.Expression typeRep) {
    if (!_generators.isNamed(type) && recordScopeDependencies(type)) {
      return typeRep;
    }
    var name = _generators._nameType(type, typeRep);
    return js.call('#()', [name]);
  }

  /// Like [nameType] but for function types.
  ///
  /// The boolean parameter [definite] distinguishes between definite function
  /// types and other types (since the same DartType may have different
  /// representations as definite and indefinite function types).
  ///
  /// The boolean parameter [lazy] indicates that the resulting expression
  /// should be a function that is invoked to compute the type, rather than the
  /// type itself. This allows better integration with `lazyFn`, avoiding an
  /// extra level of indirection.
  js_ast.Expression nameFunctionType(
      FunctionType type, js_ast.Expression typeRep,
      {bool lazy = false}) {
    if (!_generators.isNamed(type) && recordScopeDependencies(type)) {
      return lazy ? js_ast.ArrowFun([], typeRep) : typeRep;
    }

    var name = _generators._nameType(type, typeRep);
    return lazy ? name : js.call('#()', [name]);
  }
}
