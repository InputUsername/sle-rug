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
  	case normalQuestion(str label, str id, AType t, src = loc l): {
  	  tenv += { <l, id, label, atype2type(t)> };
  	}
  	case computedQuestion(str label, str id, AType t, AExpr _, src = loc l): {
  	  tenv += { <l, id, label, atype2type(t)> };
  	}
  }
  return tenv;
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
  return {}; 
}

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
  return {}; 
}

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  
  //TODO: inefficient for correct branches; labels them as twigs too.
  bool isTwig(AExpr ex, TEnv tenv, UseDef useDef){ //where twig == 'leaf branch'
    if(ex has expr) //single argument
      return typeOf(ex, tenv, useDef) != tunknown();
    if(ex has expr_lhs && ex has expr_rhs) //two arguments
      return typeOf(ex.expr_lhs, tenv, useDef) != tunknown() && typeOf(ex.expr_rhs, tenv, useDef) != tunknown();
    return false;
  }
  
  set[Message] msgs = {};
  
  //TODO: ACTUALLY USE RECURSION!!!
  
  visit (e) { //incredibly inefficient, polynomially more calls to typeOf() than with recursion
    case ref(str x, src = loc u):
      msgs += { error("Undeclared question", u) | useDef[u] == {} };
	case not(AExpr expr, src = loc u): {
	  Type texpr = typeOf(expr, tenv, useDef);
	  msgs += { error("Incorrect type of operand", u) | isTwig(e, tenv, useDef) && texpr != tbool() };
	}
	case mul(AExpr lhs, AExpr rhs, src = loc u): {
	  Type lexpr = typeOf(lhs, tenv, useDef);
	  Type rexpr = typeOf(rhs, tenv, useDef);
	  msgs += { error("Incorrect type of operands", u) | isTwig(e, tenv, useDef) && (lexpr != tint() || rexpr != tint()) };
	}
	case div(AExpr lhs, AExpr rhs, src = loc u): {
      Type lexpr = typeOf(lhs, tenv, useDef);
      Type rexpr = typeOf(rhs, tenv, useDef);
      msgs += { error("Incorrect type of operands", u) | isTwig(e, tenv, useDef) && (lexpr != tint() || rexpr != tint()) };
    }
    case add(AExpr lhs, AExpr rhs, src = loc u): {
      Type lexpr = typeOf(lhs, tenv, useDef);
      Type rexpr = typeOf(rhs, tenv, useDef);
      msgs += { error("Incorrect type of operands", u) | isTwig(e, tenv, useDef) && (lexpr != tint() || rexpr != tint()) };
    }
    case sub(AExpr lhs, AExpr rhs, src = loc u): {
      Type lexpr = typeOf(lhs, tenv, useDef);
      Type rexpr = typeOf(rhs, tenv, useDef);
      msgs += { error("Incorrect type of operands", u) | isTwig(e, tenv, useDef) && (lexpr != tint() || rexpr != tint()) };
    }
    case lt(AExpr lhs, AExpr rhs, src = loc u): {
      Type lexpr = typeOf(lhs, tenv, useDef);
      Type rexpr = typeOf(rhs, tenv, useDef);
      msgs += { error("Incorrect type of operands", u) | isTwig(e, tenv, useDef) && (lexpr != tint() || rexpr != tint()) };
    }
    
  }
  
  return msgs;
}

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
      return typeOf(expr, tenv, useDef); //don't check whether this is actually a bool or not
    default:{
      if(e has expr_lhs && e has expr_rhs){
        Type tlhs = typeOf(e.expr_lhs, tenv, useDef);
          if(tlhs == typeOf(e.expr_rhs, tenv, useDef)){
            return tlhs;
          }
      }
    }
  }
  return tunknown(); 
}

/* 
 * Pattern-based dispatch style:
 * 
 * Type typeOf(ref(str x, src = loc u), TEnv tenv, UseDef useDef) = t
 *   when <u, loc d> <- useDef, <d, x, _, Type t> <- tenv
 *
 * ... etc.
 * 
 * default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 *
 */
 
 

