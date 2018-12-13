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
  return ({} | it + check(q, tenv, useDef) | q <- f.questions);
}

set[Message] check(if_then(AExpr expr, list[AQuestion] questions, src = loc u), TEnv tenv, UseDef useDef)
  = check(expr, tenv, useDef)
  + ( {} | it + check(q, tenv, useDef) | q <- questions );

set[Message] check(if_then_else(AExpr expr, list[AQuestion] if_questions, list[AQuestion] else_questions, src = loc u), TEnv tenv, UseDef useDef)
  = check(expr, tenv, useDef)
  + ( {} | it + check(q, tenv, useDef) | q <- if_questions )
  + ( {} | it + check(q, tenv, useDef) | q <- else_questions );

set[Message] check(block(list[AQuestion] questions, src = loc u), TEnv tenv, UseDef useDef)
  = ( {} | it + check(q, tenv, useDef) | q <- questions );

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.

set[Message] check(normalQuestion(str label, str id, AType t, src = loc u), TEnv tenv, UseDef useDef)
  = check(id, t, u, tenv, useDef)
  + check(id, label, u, tenv, useDef);

set[Message] check(computedQuestion(str label, str id, AType t, AExpr expr, src = loc u), TEnv tenv, UseDef useDef)
  = { error("Declared type does not match expression type", u) | atype2type(t) != typeOf(expr, tenv, useDef) }
  + check(id, t, u, tenv, useDef)
  + check(id, label, u, tenv, useDef)
  + check(expr, tenv, useDef);

// Check if there are multiple questions with the same name but different types.
// Accepts only the question id and its type to make the function generic for
// normalQuestion and computedQuestion.
set[Message] check(str id, AType t, loc src, TEnv tenv, UseDef useDef)
  = { error("Question declared multiple times with different types", src)
    | any(<_, name, _, \type> <- tenv, id == name && atype2type(t) != \type) };

// Check if there are multiple questions with the same label but different names.
set[Message] check(str id, str label, loc src, TEnv tenv, UseDef useDef)
  = { warning("Label used multiple times for different questions", src)
    | any(<_, name, otherLabel, _> <- tenv, id != name && label == otherLabel) };

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()

set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  return checkRec(e, tenv, useDef, {});
}

set[Message] checkRec(AExpr e, TEnv tenv, UseDef useDef, set[Message] msgs) {
  bool isTwig(AExpr ex, TEnv tenv, UseDef useDef){ //where twig == 'leaf branch'
    if(ex has expr) //single argument
      return typeOf(ex, tenv, useDef) != tunknown();
    if(ex has expr_lhs && ex has expr_rhs) //two arguments
      return typeOf(ex.expr_lhs, tenv, useDef) != tunknown() && typeOf(ex.expr_rhs, tenv, useDef) != tunknown();
    return false;
  }
  
  switch(e) {
    case ref(str x, src = loc u):
      msgs += { error("Undeclared question", u) | useDef[u] == {} };
	case not(AExpr expr, src = loc u): {
	  if(isTwig(e, tenv, useDef)) {
	    Type texpr = typeOf(expr, tenv, useDef);
	    msgs += { error("Incorrect type of operand", u) | texpr != tbool() };
	  } else {
	    return check(expr, tenv, useDef);
	  }
	}
	case mul(AExpr lhs, AExpr rhs, src = loc u): {
	  if(isTwig(e, tenv, useDef)) {
	    Type lexpr = typeOf(lhs, tenv, useDef);
	    Type rexpr = typeOf(rhs, tenv, useDef);
	    msgs += { error("Incorrect type of operands", u) | lexpr != tint() || rexpr != tint() };
	  } else {
	    return check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
	  }
	}
	case div(AExpr lhs, AExpr rhs, src = loc u): {
      if(isTwig(e, tenv, useDef)) {
	    Type lexpr = typeOf(lhs, tenv, useDef);
        Type rexpr = typeOf(rhs, tenv, useDef);
        msgs += { error("Incorrect type of operands", u) | lexpr != tint() || rexpr != tint() };
      } else {
	    return check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
	  }
	}
    case add(AExpr lhs, AExpr rhs, src = loc u): {
      if(isTwig(e, tenv, useDef)) {
        Type lexpr = typeOf(lhs, tenv, useDef);
        Type rexpr = typeOf(rhs, tenv, useDef);
        msgs += { error("Incorrect type of operands", u) | lexpr != tint() || rexpr != tint() };
      } else {
	    return check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
	  }
    }
    case sub(AExpr lhs, AExpr rhs, src = loc u): {
      if(isTwig(e, tenv, useDef)) {
        Type lexpr = typeOf(lhs, tenv, useDef);
        Type rexpr = typeOf(rhs, tenv, useDef);
        msgs += { error("Incorrect type of operands", u) | lexpr != tint() || rexpr != tint() };
      } else {
	    return check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
	  }
    }
    case lt(AExpr lhs, AExpr rhs, src = loc u): {
      if(isTwig(e, tenv, useDef)) {
        Type lexpr = typeOf(lhs, tenv, useDef);
        Type rexpr = typeOf(rhs, tenv, useDef);
        msgs += { error("Incorrect type of operands", u) | lexpr != tint() || rexpr != tint() };
      } else {
	    return check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
	  }
    }
    case gt(AExpr lhs, AExpr rhs, src = loc u): {
      if(isTwig(e, tenv, useDef)) {
        Type lexpr = typeOf(lhs, tenv, useDef);
        Type rexpr = typeOf(rhs, tenv, useDef);
        msgs += { error("Incorrect type of operands", u) | lexpr != tint() || rexpr != tint() };
      } else {
	    return check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
	  }
    }
    case leq(AExpr lhs, AExpr rhs, src = loc u): {
      if(isTwig(e, tenv, useDef)) {
        Type lexpr = typeOf(lhs, tenv, useDef);
        Type rexpr = typeOf(rhs, tenv, useDef);
        msgs += { error("Incorrect type of operands", u) | lexpr != tint() || rexpr != tint() };
      } else {
	    return check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
	  }
    }
    case geq(AExpr lhs, AExpr rhs, src = loc u): {
      if(isTwig(e, tenv, useDef)) {
        Type lexpr = typeOf(lhs, tenv, useDef);
        Type rexpr = typeOf(rhs, tenv, useDef);
        msgs += { error("Incorrect type of operands", u) | lexpr != tint() || rexpr != tint() };
      } else {
	    return check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
	  }
    }
    case eq(AExpr lhs, AExpr rhs, src = loc u): {
      if(isTwig(e, tenv, useDef)) {
        Type lexpr = typeOf(lhs, tenv, useDef);
        Type rexpr = typeOf(rhs, tenv, useDef);
        msgs += { error("Incorrect type of operands", u) | lexpr != rexpr };
      } else {
	    return check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
	  }
    }
    case neq(AExpr lhs, AExpr rhs, src = loc u): {
      if(isTwig(e, tenv, useDef)) {
        Type lexpr = typeOf(lhs, tenv, useDef);
        Type rexpr = typeOf(rhs, tenv, useDef);
        msgs += { error("Incorrect type of operands", u) | lexpr != rexpr };
      } else {
	    return check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
	  }
    }
    case and(AExpr lhs, AExpr rhs, src = loc u): {
      if(isTwig(e, tenv, useDef)) {
        Type lexpr = typeOf(lhs, tenv, useDef);
        Type rexpr = typeOf(rhs, tenv, useDef);
        msgs += { error("Incorrect type of operands", u) | lexpr != tbool() || rexpr != tbool() };
      } else {
	    return check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
	  }
    }
    case or(AExpr lhs, AExpr rhs, src = loc u): {
      if(isTwig(e, tenv, useDef)) {
        Type lexpr = typeOf(lhs, tenv, useDef);
        Type rexpr = typeOf(rhs, tenv, useDef);
        msgs += { error("Incorrect type of operands", u) | lexpr != tbool() || rexpr != tint() };
      } else {
	    return check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
	  }
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
      if(e has expr_lhs && e has expr_rhs){ //assume all operators only work with one type for all operands
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
 
 

