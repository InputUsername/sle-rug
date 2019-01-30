module Check

import AST;
import Resolve;
import Message; // see standard library

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

Type atype2type(stringType()) = tstr();
Type atype2type(integerType()) = tint();
Type atype2type(booleanType()) = tbool();

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
TEnv collect(AForm f) {
  TEnv tenv = {};
  visit (f.questions) {
    case normalQuestion(str label, str id, AType t, src = loc l):
      tenv += { <l, id, label, atype2type(t)> };

    case computedQuestion(str label, str id, AType t, AExpr _, src = loc l):
      tenv += { <l, id, label, atype2type(t)> };
  }
  return tenv;
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef)
  = ( {} | it + check(q, tenv, useDef) | q <- f.questions );

set[Message] check(ifThen(AExpr expr, list[AQuestion] questions, src = loc u), TEnv tenv, UseDef useDef)
  = check(expr, tenv, useDef)
  + ( {} | it + check(q, tenv, useDef) | q <- questions );

set[Message] check(ifThenElse(AExpr expr, list[AQuestion] ifQuestions, list[AQuestion] elseQuestions, src = loc u),
                   TEnv tenv, UseDef useDef)
  = check(expr, tenv, useDef)
  + ( {} | it + check(q, tenv, useDef) | q <- ifQuestions )
  + ( {} | it + check(q, tenv, useDef) | q <- elseQuestions );

set[Message] check(block(list[AQuestion] questions, src = loc u), TEnv tenv, UseDef useDef)
  = ( {} | it + check(q, tenv, useDef) | q <- questions );

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.

set[Message] check(normalQuestion(str label, str id, AType t, src = loc u), TEnv tenv, UseDef useDef)
  = checkTypes(id, t, u, tenv, useDef)
  + checkLabels(id, label, u, tenv, useDef);

set[Message] check(computedQuestion(str label, str id, AType t, AExpr expr, src = loc u), TEnv tenv, UseDef useDef)
  = { error("Declared type does not match expression type", u) | atype2type(t) != typeOf(expr, tenv, useDef) }
  + checkTypes(id, t, u, tenv, useDef)
  + checkLabels(id, label, u, tenv, useDef)
  + check(expr, tenv, useDef);

// Check if there are multiple questions with the same name but different types.
// Accepts only the question id and its type to make the function generic for
// normalQuestion and computedQuestion.
set[Message] checkTypes(str id, AType t, loc src, TEnv tenv, UseDef useDef)
  = { error("Question declared multiple times with different types", src)
    | any(<_, name, _, \type> <- tenv, id == name && atype2type(t) != \type) };

// Check if there are multiple questions with the same label but different names.
// This should produce a warning.
set[Message] checkLabels(str id, str label, loc src, TEnv tenv, UseDef useDef)
  = { warning("Label used multiple times for different questions", src)
    | any(<_, name, otherLabel, _> <- tenv, id != name && label == otherLabel) };

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()

set[Message] check(AExpr e, TEnv tenv, UseDef useDef)
  = checkRec(e, tenv, useDef, {});

bool isTwig(AExpr ex, TEnv tenv, UseDef useDef) { // where twig == 'leaf branch'
    if (ex has expr) // single argument
      return typeOf(ex, tenv, useDef) != tunknown();
    if (ex has lhs && ex has rhs) // two arguments
      return typeOf(ex.lhs, tenv, useDef) != tunknown() && typeOf(ex.rhs, tenv, useDef) != tunknown();
    return false;
  }

set[Message] checkRecUnOpBool(AExpr e, TEnv tenv, UseDef useDef, loc u) {
  if (isTwig(e, tenv, useDef)) {
    Type texpr = typeOf(expr, tenv, useDef);
    return { error("Incorrect type of operand", u) | texpr != tbool() };
  }
  else {
    return check(expr, tenv, useDef);
  }
}

set[Message] checkRecBinOpInt(AExpr e, TEnv tenv, UseDef useDef, loc u) {
  if (isTwig(e, tenv, useDef)) {
    Type lexpr = typeOf(e.lhs, tenv, useDef);
    Type rexpr = typeOf(e.rhs, tenv, useDef);
    return { error("Incorrect type of operands", u) | lexpr != tint() || rexpr != tint() };
  }
  else {
    return check(e.lhs, tenv, useDef) + check(e.rhs, tenv, useDef);
  }
}

set[Message] checkRecBinOpBool(AExpr e, TEnv tenv, UseDef useDef, loc u) {
  if (isTwig(e, tenv, useDef)) {
    Type lexpr = typeOf(e.lhs, tenv, useDef);
    Type rexpr = typeOf(e.rhs, tenv, useDef);
    return { error("Incorrect type of operands", u) | lexpr != tbool() || rexpr != tbool() };
  }
  else {
    return check(e.lhs, tenv, useDef) + check(e.rhs, tenv, useDef);
  }
}

set[Message] checkRecBinOpAll(AExpr e, TEnv tenv, UseDef useDef, loc u) {
  if(isTwig(e, tenv, useDef)) {
    Type lexpr = typeOf(e.lhs, tenv, useDef);
    Type rexpr = typeOf(e.rhs, tenv, useDef);
    return { error("Incorrect type of operands", u) | lexpr != rexpr };
  }
  else {
    return check(e.lhs, tenv, useDef) + check(e.rhs, tenv, useDef);
  }
}

set[Message] checkRec(ref(str x, src = loc u),
                      TEnv tenv, UseDef useDef, set[Message] msgs)
  = msgs + { error("Undeclared question", u) | useDef[u] == {} };

set[Message] checkRec(integer(int _),
                      TEnv tenv, UseDef useDef, set[Message] msgs)
  = {};

set[Message] checkRec(string(str _),
                      TEnv tenv, UseDef useDef, set[Message] msgs)
  = {};
  
set[Message] checkRec(boolean(bool _),
                      TEnv tenv, UseDef useDef, set[Message] msgs)
  = {};

set[Message] checkRec(e: not(AExpr expr, src = loc u),
                      TEnv tenv, UseDef useDef, set[Message] msgs)
  = msgs + checkRecUnOpBool(e, tenv, useDef, u);

set[Message] checkRec(e: mul(AExpr lhs, AExpr rhs, src = loc u),
                     TEnv tenv, UseDef useDef, set[Message] msgs)
  = msgs + checkRecBinOpInt(e, tenv, useDef, u);

set[Message] checkRec(e: div(AExpr lhs, AExpr rhs, src = loc u),
                      TEnv tenv, UseDef useDef, set[Message] msgs)
  = msgs + checkRecBinOpInt(e, tenv, useDef, u);

set[Message] checkRec(e: add(AExpr lhs, AExpr rhs, src = loc u),
                      TEnv tenv, UseDef useDef, set[Message] msgs)
  = msgs + checkRecBinOpInt(e, tenv, useDef, u);

set[Message] checkRec(e: sub(AExpr lhs, AExpr rhs, src = loc u),
                      TEnv tenv, UseDef useDef, set[Message] msgs)
  = msgs + checkRecBinOpInt(e, tenv, useDef, u);

set[Message] checkRec(e: lt(AExpr lhs, AExpr rhs, src = loc u),
                      TEnv tenv, UseDef useDef, set[Message] msgs)
  = msgs + checkRecBinOpInt(e, tenv, useDef, u);

set[Message] checkRec(e: gt(AExpr lhs, AExpr rhs, src = loc u),
                      TEnv tenv, UseDef useDef, set[Message] msgs)
  = msgs + checkRecBinOpInt(e, tenv, useDef, u);

set[Message] checkRec(e: leq(AExpr lhs, AExpr rhs, src = loc u),
                      TEnv tenv, UseDef useDef, set[Message] msgs)
  = msgs + checkRecBinOpInt(e, tenv, useDef, u);
  
set[Message] checkRec(e: geq(AExpr lhs, AExpr rhs, src = loc u),
                      TEnv tenv, UseDef useDef, set[Message] msgs)
  = msgs + checkRecBinOpInt(e, tenv, useDef, u);

set[Message] checkRec(e: eq(AExpr lhs, AExpr rhs, src = loc u),
                      TEnv tenv, UseDef useDef, set[Message] msgs)
  = msgs + checkRecBinOpAll(e, tenv, useDef, u);

set[Message] checkRec(e: neq(AExpr lhs, AExpr rhs, src = loc u),
                      TEnv tenv, UseDef useDef, set[Message] msgs)
  = msgs + checkRecBinOpAll(e, tenv, useDef, u);
  
set[Message] checkRec(e: and(AExpr lhs, AExpr rhs, src = loc u),
                      TEnv tenv, UseDef useDef, set[Message] msgs)
  = msgs + checkRecBinOpBool(e, tenv, useDef, u);

set[Message] checkRec(e: or(AExpr lhs, AExpr rhs, src = loc u),
                      TEnv tenv, UseDef useDef, set[Message] msgs)
  = msgs + checkRecBinOpBool(e, tenv, useDef, u);
  

/* Resolves the type of the expression: propagates type upwards and checks all operands have equal types.
 * If not: returns tunknown().
 * Currently only supports certain unary operators and binary operators.
 */
Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(str x, src = loc u):  
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
      }
    case string(str _):
      return tstr();
    case integer(int _):
      return tint();
    case boolean(bool _):
      return tbool();
    case not(AExpr expr):
      return typeOf(expr, tenv, useDef); // don't check whether this is actually a bool or not
    default:{
      if (e has lhs && e has rhs) { // assume all operators only work with one type for all operands
        Type tlhs = typeOf(e.lhs, tenv, useDef);
        if (tlhs == typeOf(e.rhs, tenv, useDef)) {
          return tlhs;
        }
      }
    }
  }
  return tunknown(); 
}
