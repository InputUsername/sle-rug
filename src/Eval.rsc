module Eval

import AST;
import Resolve;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
  = input(str question, Value \value);
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) {
  return ();
}


// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) {
  return (); 
}

VEnv eval(AQuestion q, Input inp, VEnv venv) {
  // evaluate conditions for branching,
  // evaluate inp and computed questions to return updated VEnv
  return (); 
}

Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(str x): return venv[x];
    case string(str s): return vstr(s);
    case integer(int i): return vint(i);
    case boolean(bool b): return vbool(b);
    case not(AExpr expr):
      return vbool(!eval(expr, venv).b);
    case mul(AExpr lhs, AExpr rhs):
      return vint(eval(lhs, venv).n * eval(rhs, venv).n);
    case div(AExpr lhs, AExpr rhs):
      return vint(eval(lhs, venv).n / eval(rhs, venv).n);
    case add(AExpr lhs, AExpr rhs):
      return vint(eval(lhs, venv).n + eval(rhs, venv).n);
    case sub(AExpr lhs, AExpr rhs):
      return vint(eval(lhs, venv).n - eval(rhs, venv).n);
    case lt(AExpr lhs, AExpr rhs):
      return vbool(eval(lhs, venv).n < eval(rhs, venv).n);
    case gt(AExpr lhs, AExpr rhs):
      return vbool(eval(lhs, venv).n > eval(rhs, venv).n);
    case leq(AExpr lhs, AExpr rhs):
      return vbool(eval(lhs, venv).n <= eval(rhs, venv).n);
    case geq(AExpr lhs, AExpr rhs):
      return vbool(eval(lhs, venv).n >= eval(rhs, venv).n);
    case eq(AExpr lhs, AExpr rhs): {
      // Type checker makes sure typeOf(lhs) == typeOf(rhs)
      // so we only need to match on lhs
      Value vlhs = eval(lhs, venv);
      switch (vlhs) {
        case vint(int n): return vbool(n == eval(rhs, venv).n);
        case vstr(str s): return vbool(s == eval(rhs, venv).s);
        case vbool(bool b): return vbool(b == eval(rhs, venv).b);
      }
    }
    case neq(AExpr lhs, AExpr rhs): {
      // Type checker makes sure typeOf(lhs) == typeOf(rhs)
      // so we only need to match on lhs
      Value vlhs = eval(lhs, venv);
      switch (vlhs) {
        case vint(int n): return vbool(n != eval(rhs, venv).n);
        case vstr(str s): return vbool(s != eval(rhs, venv).s);
        case vbool(bool b): return vbool(b != eval(rhs, venv).b);
      }
    }
    case and(AExpr lhs, AExpr rhs):
      return vbool(eval(lhs, venv).b && eval(rhs, venv).b);
    case or(AExpr lhs, AExpr rhs):
      return vbool(eval(lhs, venv).b || eval(rhs, venv).b);
    
    default: throw "Unsupported expression <e>";
  }
}