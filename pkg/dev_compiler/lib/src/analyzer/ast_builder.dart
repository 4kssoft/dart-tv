// Copyright (c) 2015, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'package:analyzer/dart/ast/ast.dart';
import 'package:analyzer/dart/ast/standard_ast_factory.dart';
import 'package:analyzer/dart/ast/token.dart';
import 'package:analyzer/src/dart/ast/token.dart';
import 'package:analyzer/src/generated/utilities_dart.dart';

export 'package:analyzer/dart/ast/standard_ast_factory.dart';

final ast = AstBuilder();

class AstBuilder {
  KeywordToken get constKeyword => KeywordToken(Keyword.CONST, 0);

  TypeName typeName(Identifier id, List<TypeAnnotation> args) {
    TypeArgumentList argList;
    if (args != null && args.isNotEmpty) argList = typeArgumentList(args);
    return astFactory.typeName(id, argList);
  }

  FunctionTypeAlias functionTypeAlias(TypeName ret, SimpleIdentifier name,
      List<TypeParameter> tParams, List<FormalParameter> params) {
    TypeParameterList tps =
        (tParams.isEmpty) ? null : typeParameterList(tParams);
    FormalParameterList fps = formalParameterList(params);
    Token semi = Token(TokenType.SEMICOLON, 0);
    Token td = KeywordToken(Keyword.TYPEDEF, 0);
    return astFactory.functionTypeAlias(
        null, null, td, ret, name, tps, fps, semi);
  }

  Expression parenthesize(Expression exp) {
    if (exp is Identifier ||
        exp is ParenthesizedExpression ||
        exp is FunctionExpressionInvocation ||
        exp is MethodInvocation) return exp;
    return parenthesizedExpression(exp);
  }

  PropertyAccess propertyAccess(Expression target, SimpleIdentifier name) {
    var p = Token(TokenType.PERIOD, 0);
    return astFactory.propertyAccess(target, p, name);
  }

  MethodInvocation methodInvoke(Expression target, SimpleIdentifier name,
      TypeArgumentList typeArguments, NodeList<Expression> args) {
    var p = Token(TokenType.PERIOD, 0);
    return astFactory.methodInvocation(
        target, p, name, typeArguments, argumentList(args));
  }

  TokenType getTokenType(String lexeme) {
    switch (lexeme) {
      case "&":
        return TokenType.AMPERSAND;
      case "&&":
        return TokenType.AMPERSAND_AMPERSAND;
      case "&=":
        return TokenType.AMPERSAND_EQ;
      case "@":
        return TokenType.AT;
      case "!":
        return TokenType.BANG;
      case "!=":
        return TokenType.BANG_EQ;
      case "|":
        return TokenType.BAR;
      case "||":
        return TokenType.BAR_BAR;
      case "|=":
        return TokenType.BAR_EQ;
      case ":":
        return TokenType.COLON;
      case ",":
        return TokenType.COMMA;
      case "^":
        return TokenType.CARET;
      case "^=":
        return TokenType.CARET_EQ;
      case "}":
        return TokenType.CLOSE_CURLY_BRACKET;
      case ")":
        return TokenType.CLOSE_PAREN;
      case "]":
        return TokenType.CLOSE_SQUARE_BRACKET;
      case "=":
        return TokenType.EQ;
      case "==":
        return TokenType.EQ_EQ;
      case "=>":
        return TokenType.FUNCTION;
      case ">":
        return TokenType.GT;
      case ">=":
        return TokenType.GT_EQ;
      case ">>":
        return TokenType.GT_GT;
      case ">>=":
        return TokenType.GT_GT_EQ;
      case ">>>":
        return TokenType.GT_GT_GT;
      case ">>>=":
        return TokenType.GT_GT_GT_EQ;
      case "#":
        return TokenType.HASH;
      case "[]":
        return TokenType.INDEX;
      case "[]=":
        return TokenType.INDEX_EQ;
      case "<":
        return TokenType.LT;
      case "<=":
        return TokenType.LT_EQ;
      case "<<":
        return TokenType.LT_LT;
      case "<<=":
        return TokenType.LT_LT_EQ;
      case "-":
        return TokenType.MINUS;
      case "-=":
        return TokenType.MINUS_EQ;
      case "--":
        return TokenType.MINUS_MINUS;
      case "{":
        return TokenType.OPEN_CURLY_BRACKET;
      case "(":
        return TokenType.OPEN_PAREN;
      case "[":
        return TokenType.OPEN_SQUARE_BRACKET;
      case "%":
        return TokenType.PERCENT;
      case "%=":
        return TokenType.PERCENT_EQ;
      case ".":
        return TokenType.PERIOD;
      case "..":
        return TokenType.PERIOD_PERIOD;
      case "+":
        return TokenType.PLUS;
      case "+=":
        return TokenType.PLUS_EQ;
      case "++":
        return TokenType.PLUS_PLUS;
      case "?":
        return TokenType.QUESTION;
      case ";":
        return TokenType.SEMICOLON;
      case "/":
        return TokenType.SLASH;
      case "/=":
        return TokenType.SLASH_EQ;
      case "*":
        return TokenType.STAR;
      case "*=":
        return TokenType.STAR_EQ;
      case "\${":
        return TokenType.STRING_INTERPOLATION_EXPRESSION;
      case "\$":
        return TokenType.STRING_INTERPOLATION_IDENTIFIER;
      case "~":
        return TokenType.TILDE;
      case "~/":
        return TokenType.TILDE_SLASH;
      case "~/=":
        return TokenType.TILDE_SLASH_EQ;
      case "`":
        return TokenType.BACKPING;
      case "\\":
        return TokenType.BACKSLASH;
      case "...":
        return TokenType.PERIOD_PERIOD_PERIOD;
      case "??":
        return TokenType.QUESTION_QUESTION;
      case "??=":
        return TokenType.QUESTION_QUESTION_EQ;
      default:
        return null;
    }
  }

  Token _binaryOperation(String oper) {
    var type = getTokenType(oper);
    assert(type != null);
    return Token(type, 0);
  }

  BinaryExpression binaryExpression(Expression l, String oper, Expression r) {
    Token token = _binaryOperation(oper);
    return astFactory.binaryExpression(l, token, r);
  }

  ConditionalExpression conditionalExpression(
      Expression cond, Expression tExp, Expression fExp) {
    var q = Token(TokenType.QUESTION, 0);
    var c = Token(TokenType.COLON, 0);
    return astFactory.conditionalExpression(cond, q, tExp, c, fExp);
  }

  Expression application(Expression function, List<Expression> es) {
    ArgumentList args = argumentList(es);
    return functionExpressionInvocation(function, args);
  }

  Block block(List<Statement> statements) {
    Token ld = BeginToken(TokenType.OPEN_CURLY_BRACKET, 0);
    Token rd = Token(TokenType.CLOSE_CURLY_BRACKET, 0);
    return astFactory.block(ld, statements, rd);
  }

  MethodDeclaration blockMethodDeclaration(TypeName rt, SimpleIdentifier m,
      List<FormalParameter> params, List<Statement> statements,
      {bool isStatic = false}) {
    FormalParameterList fl = formalParameterList(params);
    Block b = block(statements);
    BlockFunctionBody body = blockFunctionBody(b);
    return methodDeclaration(rt, m, fl, body, isStatic: isStatic);
  }

  FunctionDeclaration blockFunctionDeclaration(TypeName rt, SimpleIdentifier f,
      List<FormalParameter> params, List<Statement> statements) {
    FunctionExpression fexp = blockFunction(params, statements);
    return functionDeclaration(rt, f, fexp);
  }

  FunctionExpression blockFunction(
      List<FormalParameter> params, List<Statement> statements) {
    FormalParameterList fl = formalParameterList(params);
    Block b = block(statements);
    BlockFunctionBody body = blockFunctionBody(b);
    return functionExpression(fl, body);
  }

  FunctionExpression expressionFunction(
      List<FormalParameter> params, Expression body,
      [bool decl = false]) {
    FormalParameterList fl = formalParameterList(params);
    ExpressionFunctionBody b = expressionFunctionBody(body, decl);
    return functionExpression(fl, b);
  }

  FunctionDeclarationStatement functionDeclarationStatement(
      TypeName rType, SimpleIdentifier name, FunctionExpression fe) {
    var fd = functionDeclaration(rType, name, fe);
    return astFactory.functionDeclarationStatement(fd);
  }

  // let b = e1 in e2 == (\b.e2)(e1)
  Expression letExpression(FormalParameter b, Expression e1, Expression e2) {
    FunctionExpression l = expressionFunction(<FormalParameter>[b], e2);
    return application(parenthesize(l), <Expression>[e1]);
  }

  FormalParameter requiredFormal(NormalFormalParameter fp) {
    return requiredFormalParameter(fp);
  }

  FormalParameter optionalFormal(NormalFormalParameter fp) {
    return optionalFormalParameter(fp);
  }

  FormalParameter namedFormal(NormalFormalParameter fp) {
    return namedFormalParameter(fp);
  }

  NamedExpression namedParameter(String s, Expression e) {
    return namedExpression(s, e);
  }

  NamedExpression namedExpression(String s, Expression e) {
    Label l =
        astFactory.label(identifierFromString(s), Token(TokenType.COLON, 0));
    return astFactory.namedExpression(l, e);
  }

  /// Declares a single variable `var <name> = <init>` with the type and name
  /// specified by the VariableElement. See also [variableStatement].
  VariableDeclarationList declareVariable(SimpleIdentifier name,
      [Expression init]) {
    var eqToken = init != null ? Token(TokenType.EQ, 0) : null;
    var varToken = KeywordToken(Keyword.VAR, 0);
    return astFactory.variableDeclarationList(null, null, varToken, null,
        [astFactory.variableDeclaration(name, eqToken, init)]);
  }

  VariableDeclarationStatement variableStatement(SimpleIdentifier name,
      [Expression init]) {
    return variableDeclarationStatement(declareVariable(name, init));
  }

  InstanceCreationExpression instanceCreation(
      ConstructorName ctor, List<Expression> args) {
    var newToken = KeywordToken(Keyword.NEW, 0);
    return astFactory.instanceCreationExpression(
        newToken, ctor, argumentList(args));
  }

  ConstructorName constructorName(TypeName type, [SimpleIdentifier name]) {
    Token period = name != null ? Token(TokenType.PERIOD, 0) : null;
    return astFactory.constructorName(type, period, name);
  }

  SimpleIdentifier identifierFromString(String name) {
    StringToken token = SyntheticStringToken(TokenType.IDENTIFIER, name, 0);
    return astFactory.simpleIdentifier(token);
  }

  PrefixedIdentifier prefixedIdentifier(
      SimpleIdentifier pre, SimpleIdentifier id) {
    Token period = Token(TokenType.PERIOD, 0);
    return astFactory.prefixedIdentifier(pre, period, id);
  }

  TypeParameter typeParameter(SimpleIdentifier name, [TypeName bound]) {
    Token keyword = (bound == null) ? null : KeywordToken(Keyword.EXTENDS, 0);
    return astFactory.typeParameter(null, null, name, keyword, bound);
  }

  TypeParameterList typeParameterList(List<TypeParameter> params) {
    Token lb = Token(TokenType.LT, 0);
    Token rb = Token(TokenType.GT, 0);
    return astFactory.typeParameterList(lb, params, rb);
  }

  TypeArgumentList typeArgumentList(List<TypeAnnotation> args) {
    Token lb = Token(TokenType.LT, 0);
    Token rb = Token(TokenType.GT, 0);
    return astFactory.typeArgumentList(lb, args, rb);
  }

  ArgumentList argumentList(List<Expression> args) {
    Token lp = BeginToken(TokenType.OPEN_PAREN, 0);
    Token rp = Token(TokenType.CLOSE_PAREN, 0);
    return astFactory.argumentList(lp, args, rp);
  }

  BooleanLiteral booleanLiteral(bool b) {
    var k = KeywordToken(b ? Keyword.TRUE : Keyword.FALSE, 0);
    return astFactory.booleanLiteral(k, b);
  }

  NullLiteral nullLiteral() {
    var n = KeywordToken(Keyword.NULL, 0);
    return astFactory.nullLiteral(n);
  }

  IntegerLiteral integerLiteral(int i) {
    StringToken token = StringToken(TokenType.INT, '$i', 0);
    return astFactory.integerLiteral(token, i);
  }

  SimpleStringLiteral simpleStringLiteral(String s) {
    StringToken token = StringToken(TokenType.STRING, "\"" + s + "\"", 0);
    return astFactory.simpleStringLiteral(token, s);
  }

  SimpleStringLiteral tripleQuotedStringLiteral(String s) {
    StringToken token = StringToken(TokenType.STRING, '"""' + s + '"""', 0);
    return astFactory.simpleStringLiteral(token, s);
  }

  AsExpression asExpression(Expression exp, TypeName type) {
    Token token = KeywordToken(Keyword.AS, 0);
    return astFactory.asExpression(exp, token, type);
  }

  IsExpression isExpression(Expression exp, TypeName type) {
    Token token = KeywordToken(Keyword.IS, 0);
    return astFactory.isExpression(exp, token, null, type);
  }

  ParenthesizedExpression parenthesizedExpression(Expression exp) {
    Token lp = BeginToken(TokenType.OPEN_PAREN, exp.offset);
    Token rp = Token(TokenType.CLOSE_PAREN, exp.end);
    return astFactory.parenthesizedExpression(lp, exp, rp);
  }

  Expression functionExpressionInvocation(
      Expression function, ArgumentList es) {
    return astFactory.functionExpressionInvocation(function, null, es);
  }

  FormalParameterList formalParameterList(List<FormalParameter> params) {
    Token lp = BeginToken(TokenType.OPEN_PAREN, 0);
    Token rp = Token(TokenType.CLOSE_PAREN, 0);
    bool hasOptional = params.any((p) => p.isPositional);
    bool hasNamed = params.any((p) => p.isNamed);
    assert(!(hasOptional && hasNamed));
    Token ld;
    Token rd;
    if (hasOptional) {
      ld = BeginToken(TokenType.OPEN_SQUARE_BRACKET, 0);
      rd = Token(TokenType.CLOSE_SQUARE_BRACKET, 0);
    }
    if (hasNamed) {
      ld = BeginToken(TokenType.OPEN_CURLY_BRACKET, 0);
      rd = Token(TokenType.CLOSE_CURLY_BRACKET, 0);
    }
    return astFactory.formalParameterList(lp, params, ld, rd, rp);
  }

  BlockFunctionBody blockFunctionBody(Block b) {
    return astFactory.blockFunctionBody(null, null, b);
  }

  ExpressionFunctionBody expressionFunctionBody(Expression body,
      [bool decl = false]) {
    Token semi = (decl) ? Token(TokenType.SEMICOLON, 0) : null;
    return astFactory.expressionFunctionBody(null, null, body, semi);
  }

  ExpressionStatement expressionStatement(Expression expression) {
    Token semi = Token(TokenType.SEMICOLON, 0);
    return astFactory.expressionStatement(expression, semi);
  }

  FunctionDeclaration functionDeclaration(
      TypeName rt, SimpleIdentifier f, FunctionExpression fexp) {
    return astFactory.functionDeclaration(null, null, null, rt, null, f, fexp);
  }

  MethodDeclaration methodDeclaration(TypeName rt, SimpleIdentifier m,
      FormalParameterList fl, FunctionBody body,
      {bool isStatic = false}) {
    Token st = isStatic ? KeywordToken(Keyword.STATIC, 0) : null;
    return astFactory.methodDeclaration(
        null, null, null, st, rt, null, null, m, null, fl, body);
  }

  FunctionExpression functionExpression(
      FormalParameterList fl, FunctionBody body) {
    return astFactory.functionExpression(null, fl, body);
  }

  Statement returnExpression([Expression e]) {
    Token ret = KeywordToken(Keyword.RETURN, 0);
    Token semi = Token(TokenType.SEMICOLON, 0);
    return astFactory.returnStatement(ret, e, semi);
  }

  FormalParameter requiredFormalParameter(NormalFormalParameter fp) {
    return fp;
  }

  FormalParameter optionalFormalParameter(NormalFormalParameter fp) {
    return astFactory.defaultFormalParameter(
        fp, ParameterKind.POSITIONAL, null, null);
  }

  FormalParameter namedFormalParameter(NormalFormalParameter fp) {
    return astFactory.defaultFormalParameter(
        fp, ParameterKind.NAMED, null, null);
  }

  VariableDeclarationStatement variableDeclarationStatement(
      VariableDeclarationList varDecl) {
    var semi = Token(TokenType.SEMICOLON, 0);
    return astFactory.variableDeclarationStatement(varDecl, semi);
  }
}
