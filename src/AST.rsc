module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  = form(str name, list[AQuestion] questions)
  ; 

data AQuestion(loc src = |tmp:///|)
  = normalQuestion(str label, str id, AType t)
  | computedQuestion(str label, str id, AType t, AExpr expr)
  | block(list[AQuestion] qs)
  | if_then(AExpr expr, list[AQuestion] qs)
  | if_then_else(AExpr expr, list[AQuestion] if_qs, list[AQuestion] else_qs)
  ;
 
data AExpr(loc src = |tmp:///|)
  = ref(str name)
  | string(str s)
  | integer(int i)
  | boolean(bool b)
  | not(AExpr expr)
  | mul(AExpr expr_lhs, AExpr expr_rhs)
  | div(AExpr expr_lhs, AExpr expr_rhs)
  | add(AExpr expr_lhs, AExpr expr_rhs)
  | sub(AExpr expr_lhs, AExpr expr_rhs)
  | lt(AExpr expr_lhs, AExpr expr_rhs)
  | gt(AExpr expr_lhs, AExpr expr_rhs)
  | leq(AExpr expr_lhs, AExpr expr_rhs)
  | geq(AExpr expr_lhs, AExpr expr_rhs)
  | eq(AExpr expr_lhs, AExpr expr_rhs)
  | neq(AExpr expr_lhs, AExpr expr_rhs)
  | and(AExpr expr_lhs, AExpr expr_rhs)
  | or(AExpr expr_lhs, AExpr expr_rhs)
  ;

data AType(loc src = |tmp:///|)
  = stringType()
  | booleanType()
  | integerType();
