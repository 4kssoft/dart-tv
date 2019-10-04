# AOT code size analysis

The Dart VM's AOT compiler has support for emitting binary size information
for all the code that gets generated. This information can then be visualized.

## Telling the AOT compiler to generate binary size information

Our AOT compiler accepts an extra `--print-instructions-sizes-to=sizes.json`
flag. If supplied the AOT compiler will emit binary size information for all
generated functions to `sizes.json`.

This flag can be passed to `gen_snapshot` directly, or to the various wrapper
scripts (e.g. `pkg/vm/tool/precompiler2`):

```
% tools/build.py -mrelease -ax64 runtime_kernel dart_precompiled_runtime
% pkg/vm/tool/precompiler2 --print-instructions-sizes-to=hello_sizes.json hello.dart hello.dart.aot
```

## Visualizing the information from the binary size json file

To visualize the information emitted by the AOT compiler one can use our binary
size analysis tool:

```
% dart pkg/vm/bin/run_binary_size_analysis.dart hello_sizes.json hello_sizes
Generated file:///.../sdk/hello_sizes/index.html
% chrome hello_sizes/index.html
```

