#include "llvm_codegen.h"

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <iostream>
#include <string>

int main(int argv, char** argc) {
  llvm::LLVMContext context;
  llvm::Module module("Main", context);
  auto program = dart_llvm::ILDeserializer::Deserialize(argc[1]);
  dart_llvm::CodegenModule cgm(module, program.get());
  cgm.GenerateProgram();

  std::string str;
  llvm::raw_string_ostream OS(str);
  OS << cgm.GetModule();
  OS.flush();
  std::cout << str << std::endl;
  return 0;
}
