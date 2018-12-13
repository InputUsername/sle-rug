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

Value defaultValue(stringType()) = vstr("");

Value defaultValue(booleanType()) = vbool(false);

Value defaultValue(integerType()) = vint(0);

VEnv initialEnv(normalQuestion(str _, str id, AType t))
  = (id: defaultValue(t));

VEnv initialEnv(computedQuestion(str _, str id, AType t, AExpr _))
  = (id: defaultValue(t));


VEnv initialEnv(block(list[AQuestion] qs))
  = ( () | it + initialEnv(q) | q <- qs);

VEnv initialEnv(if_then(AExpr _, list[AQuestion] qs))
  = ( () | it + initialEnv(q) | q <- qs);
  
VEnv initialEnv(if_then_else(AExpr _, list[AQuestion] if_qs, list[AQuestion] else_qs))
  = ( () | it + initialEnv(q) | q <- if_qs)
  + ( () | it + initialEnv(q) | q <- else_qs);

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
    
    // etc.
    
    default: throw "Unsupported expression <e>";
  }
}