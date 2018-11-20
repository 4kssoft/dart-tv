// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef RUNTIME_VM_COMPILER_BACKEND_LLVM_COMMON_DEFS_H_
#define RUNTIME_VM_COMPILER_BACKEND_LLVM_COMMON_DEFS_H_

#define FOR_EACH_SUPPORTED_INSTRUCTION(M)                                      \
  M(GraphEntry, kNoGC)                                                         \
  M(JoinEntry, kNoGC)                                                          \
  M(TargetEntry, kNoGC)                                                        \
  M(FunctionEntry, kNoGC)                                                      \
  M(Phi, kNoGC)                                                                \
  M(Parameter, kNoGC)                                                          \
  M(Return, kNoGC)                                                             \
  M(Goto, kNoGC)                                                               \
  M(Branch, kNoGC)                                                             \
  M(RelationalOp, kNoGC)                                                       \
  M(CheckStackOverflow, _)                                                     \
  M(Constant, kNoGC)                                                           \
  M(UnboxedConstant, kNoGC)                                                    \
  M(BoxInt64, _)                                                               \
  M(UnboxInt64, kNoGC)                                                         \
  M(CheckNull, kNoGC)                                                          \
  M(PushArgument, kNoGC)                                                       \
  M(StaticCall, _)                                                             \
  M(BinaryInt64Op, kNoGC)                                                      \
  M(LoadIndexedUnsafe, kNoGC)                                                  \
  M(SpecialParameter, kNoGC)                                                   \
  M(InstanceCall, kNoGC)                                                       \
  M(EqualityCompare, KNoGC)                                                    \
  M(StrictCompare, KNoGC)                                                      \
  M(PolymorphicInstanceCall, _)                                                \
  M(LoadStaticField, kNoGC)                                                    \
  M(LoadClassId, kNoGC)                                                        \
  M(AssertBoolean, _)                                                          \
  M(CheckedSmiComparison, _)                                                   \
  M(AssertAssignable, _)                                                       \
  M(AllocateObject, _)                                                         \
  M(CreateArray, _)                                                            \
  M(LoadField, kNoGC)                                                          \
  M(StoreInstanceField, kNoGC)                                                 \
  M(LoadIndexed, kNoGC)                                                        \
  M(StoreIndexed, kNoGC)                                                       \
  M(GenericCheckBound, kNoGC)                                                  \
  M(OneByteStringFromCharCode, kNoGC)                                          \
  M(StoreStaticField, kNoGC)                                                   \
  M(BooleanNegate, kNoGC)                                                      \
  M(IfThenElse, kNoGC)

namespace dart_llvm {

enum Representation {
  kNoRepresentation,
  kTagged,
  kUntagged,
  kUnboxedDouble,
  kUnboxedInt32,
  kUnboxedUint32,
  kUnboxedInt64,
  kUnboxedFloat32x4,
  kUnboxedInt32x4,
  kUnboxedFloat64x2,
  kPairOfTagged,
  kNumRepresentations
};

enum RelationalOpCid { kInt64, kDouble };

#define DART_TOKEN_LIST(TOK)                                                   \
  TOK(kEOS, "", 0, kNoAttribute)                                               \
                                                                               \
  TOK(kLPAREN, "(", 0, kNoAttribute)                                           \
  TOK(kRPAREN, ")", 0, kNoAttribute)                                           \
  TOK(kLBRACK, "[", 0, kNoAttribute)                                           \
  TOK(kRBRACK, "]", 0, kNoAttribute)                                           \
  TOK(kLBRACE, "{", 0, kNoAttribute)                                           \
  TOK(kRBRACE, "}", 0, kNoAttribute)                                           \
  TOK(kARROW, "=>", 0, kNoAttribute)                                           \
  TOK(kCOLON, ":", 0, kNoAttribute)                                            \
  TOK(kSEMICOLON, ";", 0, kNoAttribute)                                        \
  TOK(kPERIOD, ".", 0, kNoAttribute)                                           \
  TOK(kQM_PERIOD, "?.", 0, kNoAttribute)                                       \
  TOK(kINCR, "++", 0, kNoAttribute)                                            \
  TOK(kDECR, "--", 0, kNoAttribute)                                            \
                                                                               \
  /* Assignment operators.                            */                       \
  /* Please update IsAssignmentOperator() if you make */                       \
  /* any changes to this block.                       */                       \
  TOK(kASSIGN, "=", 2, kNoAttribute)                                           \
  TOK(kASSIGN_OR, "|=", 2, kNoAttribute)                                       \
  TOK(kASSIGN_XOR, "^=", 2, kNoAttribute)                                      \
  TOK(kASSIGN_AND, "&=", 2, kNoAttribute)                                      \
  TOK(kASSIGN_SHL, "<<=", 2, kNoAttribute)                                     \
  TOK(kASSIGN_SHR, ">>=", 2, kNoAttribute)                                     \
  TOK(kASSIGN_ADD, "+=", 2, kNoAttribute)                                      \
  TOK(kASSIGN_SUB, "-=", 2, kNoAttribute)                                      \
  TOK(kASSIGN_MUL, "*=", 2, kNoAttribute)                                      \
  TOK(kASSIGN_TRUNCDIV, "~/=", 2, kNoAttribute)                                \
  TOK(kASSIGN_DIV, "/=", 2, kNoAttribute)                                      \
  TOK(kASSIGN_MOD, "%=", 2, kNoAttribute)                                      \
  /* Avoid trigraph ??= below. */                                              \
  TOK(kASSIGN_COND, "?\?=", 2, kNoAttribute)                                   \
                                                                               \
  TOK(kCASCADE, "..", 2, kNoAttribute)                                         \
                                                                               \
  TOK(kCOMMA, ",", 1, kNoAttribute)                                            \
  TOK(kOR, "||", 5, kNoAttribute)                                              \
  TOK(kAND, "&&", 6, kNoAttribute)                                             \
  TOK(kBIT_OR, "|", 9, kNoAttribute)                                           \
  TOK(kBIT_XOR, "^", 10, kNoAttribute)                                         \
  TOK(kBIT_AND, "&", 11, kNoAttribute)                                         \
  TOK(kBIT_NOT, "~", 0, kNoAttribute)                                          \
                                                                               \
  /* Shift operators. */                                                       \
  TOK(kSHL, "<<", 12, kNoAttribute)                                            \
  TOK(kSHR, ">>", 12, kNoAttribute)                                            \
                                                                               \
  /* Additive operators. */                                                    \
  TOK(kADD, "+", 13, kNoAttribute)                                             \
  TOK(kSUB, "-", 13, kNoAttribute)                                             \
                                                                               \
  /* Multiplicative operators */                                               \
  TOK(kMUL, "*", 14, kNoAttribute)                                             \
  TOK(kDIV, "/", 14, kNoAttribute)                                             \
  TOK(kTRUNCDIV, "~/", 14, kNoAttribute)                                       \
  TOK(kMOD, "%", 14, kNoAttribute)                                             \
                                                                               \
  TOK(kNOT, "!", 0, kNoAttribute)                                              \
  TOK(kCONDITIONAL, "?", 3, kNoAttribute)                                      \
  TOK(kIFNULL, "??", 4, kNoAttribute)                                          \
                                                                               \
  /* Equality operators.                             */                        \
  /* Please update IsEqualityOperator() if you make  */                        \
  /* any changes to this block.                      */                        \
  TOK(kEQ, "==", 7, kNoAttribute)                                              \
  TOK(kNE, "!=", 7, kNoAttribute)                                              \
  TOK(kEQ_STRICT, "===", 7, kNoAttribute)                                      \
  TOK(kNE_STRICT, "!==", 7, kNoAttribute)                                      \
                                                                               \
  /* Relational operators.                             */                      \
  /* Please update IsRelationalOperator() if you make  */                      \
  /* any changes to this block.                        */                      \
  TOK(kLT, "<", 8, kNoAttribute)                                               \
  TOK(kGT, ">", 8, kNoAttribute)                                               \
  TOK(kLTE, "<=", 8, kNoAttribute)                                             \
  TOK(kGTE, ">=", 8, kNoAttribute)                                             \
                                                                               \
  /* Internal token for !(expr is Type) negative type test operator */         \
  TOK(kISNOT, "", 11, kNoAttribute)                                            \
                                                                               \
  TOK(kINDEX, "[]", 0, kNoAttribute)                                           \
  TOK(kASSIGN_INDEX, "[]=", 0, kNoAttribute)                                   \
  TOK(kNEGATE, "unary-", 0, kNoAttribute)                                      \
                                                                               \
  TOK(kIDENT, "", 0, kNoAttribute)                                             \
  TOK(kSTRING, "", 0, kNoAttribute)                                            \
  TOK(kINTEGER, "", 0, kNoAttribute)                                           \
  TOK(kDOUBLE, "", 0, kNoAttribute)                                            \
                                                                               \
  TOK(kINTERPOL_VAR, "$", 0, kNoAttribute)                                     \
  TOK(kINTERPOL_START, "${", 0, kNoAttribute)                                  \
  TOK(kINTERPOL_END, "}", 0, kNoAttribute)                                     \
                                                                               \
  TOK(kAT, "@", 0, kNoAttribute)                                               \
  TOK(kHASH, "#", 0, kNoAttribute)                                             \
                                                                               \
  TOK(kNEWLINE, "\n", 0, kNoAttribute)                                         \
  TOK(kWHITESP, "", 0, kNoAttribute)                                           \
  TOK(kERROR, "", 0, kNoAttribute)                                             \
  TOK(kILLEGAL, "", 0, kNoAttribute)                                           \
                                                                               \
  /* Support for Dart scripts. */                                              \
  TOK(kSCRIPTTAG, "#!", 0, kNoAttribute)                                       \
                                                                               \
  /* Support for optimized code */                                             \
  TOK(kREM, "", 0, kNoAttribute)

// List of keywords. The list must be alphabetically ordered. The
// keyword recognition code depends on the ordering.
// If you add a keyword at the beginning or end of this list, make sure
// to update kFirstKeyword and kLastKeyword below.
#define DART_KEYWORD_LIST(KW)                                                  \
  KW(kABSTRACT, "abstract", 0, kPseudoKeyword) /* == kFirstKeyword */          \
  KW(kAS, "as", 11, kPseudoKeyword)                                            \
  KW(kASSERT, "assert", 0, kKeyword)                                           \
  KW(kBREAK, "break", 0, kKeyword)                                             \
  KW(kCASE, "case", 0, kKeyword)                                               \
  KW(kCATCH, "catch", 0, kKeyword)                                             \
  KW(kCLASS, "class", 0, kKeyword)                                             \
  KW(kCONST, "const", 0, kKeyword)                                             \
  KW(kCONTINUE, "continue", 0, kKeyword)                                       \
  KW(kCOVARIANT, "covariant", 0, kPseudoKeyword)                               \
  KW(kDEFAULT, "default", 0, kKeyword)                                         \
  KW(kDEFERRED, "deferred", 0, kPseudoKeyword)                                 \
  KW(kDO, "do", 0, kKeyword)                                                   \
  KW(kELSE, "else", 0, kKeyword)                                               \
  KW(kENUM, "enum", 0, kKeyword)                                               \
  KW(kEXPORT, "export", 0, kPseudoKeyword)                                     \
  KW(kEXTENDS, "extends", 0, kKeyword)                                         \
  KW(kEXTERNAL, "external", 0, kPseudoKeyword)                                 \
  KW(kFACTORY, "factory", 0, kPseudoKeyword)                                   \
  KW(kFALSE, "false", 0, kKeyword)                                             \
  KW(kFINAL, "final", 0, kKeyword)                                             \
  KW(kFINALLY, "finally", 0, kKeyword)                                         \
  KW(kFOR, "for", 0, kKeyword)                                                 \
  KW(kGET, "get", 0, kPseudoKeyword)                                           \
  KW(kIF, "if", 0, kKeyword)                                                   \
  KW(kIMPLEMENTS, "implements", 0, kPseudoKeyword)                             \
  KW(kIMPORT, "import", 0, kPseudoKeyword)                                     \
  KW(kIN, "in", 0, kKeyword)                                                   \
  KW(kIS, "is", 11, kKeyword)                                                  \
  KW(kLIBRARY, "library", 0, kPseudoKeyword)                                   \
  KW(kNEW, "new", 0, kKeyword)                                                 \
  KW(kNULL, "null", 0, kKeyword)                                               \
  KW(kOPERATOR, "operator", 0, kPseudoKeyword)                                 \
  KW(kPART, "part", 0, kPseudoKeyword)                                         \
  KW(kRETHROW, "rethrow", 0, kKeyword)                                         \
  KW(kRETURN, "return", 0, kKeyword)                                           \
  KW(kSET, "set", 0, kPseudoKeyword)                                           \
  KW(kSTATIC, "static", 0, kPseudoKeyword)                                     \
  KW(kSUPER, "super", 0, kKeyword)                                             \
  KW(kSWITCH, "switch", 0, kKeyword)                                           \
  KW(kTHIS, "this", 0, kKeyword)                                               \
  KW(kTHROW, "throw", 0, kKeyword)                                             \
  KW(kTRUE, "true", 0, kKeyword)                                               \
  KW(kTRY, "try", 0, kKeyword)                                                 \
  KW(kTYPEDEF, "typedef", 0, kPseudoKeyword)                                   \
  KW(kVAR, "var", 0, kKeyword)                                                 \
  KW(kVOID, "void", 0, kKeyword)                                               \
  KW(kWHILE, "while", 0, kKeyword)                                             \
  KW(kWITH, "with", 0, kKeyword) /* == kLastKeyword */

#define T(t, s, p, a) t,
enum TokenKind { DART_TOKEN_LIST(T) DART_KEYWORD_LIST(T) kNumTokens };
#undef T

enum ArgcTagBits {
  kArgcBit = 0,
  kArgcSize = 24,
  kFunctionBit = kArgcBit + kArgcSize,
  kFunctionSize = 3,
  kReverseArgOrderBit = kFunctionBit + kFunctionSize,
  kReverseArgOrderSize = 1,
};

}  // namespace dart_llvm

#endif  // RUNTIME_VM_COMPILER_BACKEND_LLVM_COMMON_DEFS_H_
