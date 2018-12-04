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

Use uses(AExpr expr) {
  Use use = {};
  switch(expr){
    case r: ref(str name):
    {
      use += {<r.src, name>};
    }
    case not(AExpr e): {
      use += uses(e);
    }
    default: {
      if(expr has expr_lhs && expr has expr_rhs){
        use += uses(expr.expr_lhs) + uses(expr.expr_rhs);
      }
    }
  }
  
  return use;
}

Use uses(AForm f) {
  Use use = {};
  for(/AQuestion q := f) {
    switch(q){
      case computedQuestion(str _, str _, AType _, AExpr expr): {
        use += uses(expr);
      }
      case if_then(AExpr expr, list[AQuestion] _):{
        use += uses(expr);
      }
      case if_then_else(AExpr expr, list[AQuestion] _, list[AQuestion] _):{
        use += uses(expr);
      }
    }
  }
  return use; 
}

Def defs(AForm f) {
  
  Def def = {};
  for (/AQuestion q := f) {
    if(q has id){
      def += {<q.id, q.src>};
    }
  }
  return def; 
}