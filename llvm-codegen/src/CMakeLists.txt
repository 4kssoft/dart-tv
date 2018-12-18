add_library(dispatch_table dispatch_table.cc)
add_library(llvm_codegen llvm_codegen.cc)
add_library(il_deserializer il_deserializer.cc)
add_executable(main main.cc)

target_link_libraries(main -fsanitize=address llvm_codegen il_deserializer dispatch_table ${llvm_libs})
