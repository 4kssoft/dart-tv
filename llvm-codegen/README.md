# How to use LLVM backend

## Prerequisites

You will need LLVM development headers and libraries. Current version of 
the backend is tested to work against `01b595c0cb78764384be769236af2e23cc11ab52` of 
`llvm-project` built and installed globally. 

## Building

We are assuming that you have functional Dart SDK checkout, which can be
obtained by doing `fetch dart` and you checked out `llvm-experiment` branch.

The following targets need to built:

* `vm_platform` - provides Kernel binary with Dart SDK libraries;
* `gen_snapshot` - provides Dart AOT compiler;
* `dart_precompiled_runtime` - provides Dart AOT runtime.

```console
$ tools/build.py -m product -a x64 vm_platform gen_snapshot dart_precompiled_runtime
```

Note: specifying `product` creates a _product build_ which is considered the
most minimal runtime environment, excluding features like vmservice, debugging
and profiling support. You can also use normal `release` and `debug` builds
for debugging.

Additionally you will need to build LLVM backend, which is currently not
integrated into GN build:

```console
$ mkdir out/llvm-codegen
$ cd out/llvm-codegen
$ cmake -DTARGET_ARCH=X64 -G Ninja ../../llvm-codegen
$ ninja
```

## Testing

Currently LLVM backend supports lowering of a subset of IL instructions 
which Dart VM AOT compiler can generate. Thus is can't yet compile any 
application in its entirety. Instead functions that you would like to
compile through LLVM backend need to be annotated with `@pragma('vm:llvm')`.

Consider the following input file `/tmp/hello.dart`:

```dart
@pragma('vm:llvm')          // compile this function through LLVM
@pragma('vm:never-inline')  // prevent inlining of this function into main
int sum(int start, int end) {
  final list = <int>[];
  for (var i = start; i < end; i++) list.add(i * i);

  var sum = 0;
  for (var elem in list) {
    sum += elem;
  }
  return sum;
}

void main(List<String> args) {
  final result = sum(int.parse(args[0]), int.parse(args[1]));
  print('sum: ${result}');
}
```

This file can be compiled using `pkg/vm/tool/release_llvm_precompiler` like so:

```console
$ DART_CONFIGURATION=ProductX64 pkg/vm/tool/release_llvm_precompiler /tmp/hello
# This would produce /tmp/hello.so which can be run using precompiled runtime
$ out/ProductX64/dart_precompiled_runtime --llvm-mode /tmp/hello.so
```

Note: `.dart` is removed. 

You can pass `-v` or `--verbose` to see the steps that the script is executing.

Script generates the following interpediate files next to the input file:

* `hello.il` the serialized IL;
* `hello.llvm` the LLVM IR in text format;
* `hello.s` the corresponding assembly;
* `hello.S` the assembly from the Dart AOT pipeline;
* `hello.dill` Kernel binary (AOT optimized);
* `hello.so` library that contains all necessary snapshots and code

