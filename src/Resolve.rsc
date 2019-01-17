module Resolve

import AST;

/*
 * Name resolution for QL
 */ 


// modeling declaring occurrences of names
alias Def = rel[str name, loc def];

// modeling use occurrences of names
alias Use = rel[loc use, str name];

// the reference graph
alias UseDef = rel[loc use, loc def];

UseDef resolve(AForm f) = uses(f) o defs(f);

// Return the set of uses in an expression
Use uses(AExpr expr)
  = { <r.src, name> | /r: ref(str name) := expr };

Use uses(AForm f) {
  Use use = {};
  for (/AQuestion q := f) {
    switch (q) {
      case computedQuestion(str _, str _, AType _, AExpr expr): use += uses(expr);
      case ifThen(AExpr expr, list[AQuestion] _): use += uses(expr);
      case ifThenElse(AExpr expr, list[AQuestion] _, list[AQuestion] _): use += uses(expr);
    }
  }
  return use; 
}

Def defs(AForm f) {
  Def def = {};
  for (/AQuestion q := f) {
    if (q has id){
      def += {<q.id, q.src>};
    }
  }
  return def; 
}