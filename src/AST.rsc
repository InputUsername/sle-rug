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
  | block(list[AQuestion] questions)
  | ifThen(AExpr expr, list[AQuestion] questions)
  | ifThenElse(AExpr expr, list[AQuestion] ifQuestions, list[AQuestion] elseQuestions)
  ;
 
data AExpr(loc src = |tmp:///|)
  = ref(str name)
  | string(str s)
  | integer(int i)
  | boolean(bool b)
  | not(AExpr expr)
  | mul(AExpr lhs, AExpr rhs)
  | div(AExpr lhs, AExpr rhs)
  | add(AExpr lhs, AExpr rhs)
  | sub(AExpr lhs, AExpr rhs)
  | lt(AExpr lhs, AExpr rhs)
  | gt(AExpr lhs, AExpr rhs)
  | leq(AExpr lhs, AExpr rhs)
  | geq(AExpr lhs, AExpr rhs)
  | eq(AExpr lhs, AExpr rhs)
  | neq(AExpr lhs, AExpr rhs)
  | and(AExpr lhs, AExpr rhs)
  | or(AExpr lhs, AExpr rhs)
  ;

data AType(loc src = |tmp:///|)
  = stringType()
  | booleanType()
  | integerType();
