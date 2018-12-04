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
  switch(expr){  //TODO: think about abstracting binary operators
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
    //case mul(AExpr expr_lhs, AExpr expr_rhs): {
    //  use += uses(expr_lhs) + uses(expr_rhs);
    //}
    //case div(AExpr expr_lhs, AExpr expr_rhs): {
    //  use += uses(expr_lhs) + uses(expr_rhs);
    //}
    //case add(AExpr expr_lhs, AExpr expr_rhs): {
    //  use += uses(expr_lhs) + uses(expr_rhs);
    //}
    //case sub(AExpr expr_lhs, AExpr expr_rhs): {
    //  use += uses(expr_lhs) + uses(expr_rhs);
    //}
    //case lt(AExpr expr_lhs, AExpr expr_rhs): {
    //  use += uses(expr_lhs) + uses(expr_rhs);
    //}
    //case gt(AExpr expr_lhs, AExpr expr_rhs): {
    //  use += uses(expr_lhs) + uses(expr_rhs);
    //}
    //case leq(AExpr expr_lhs, AExpr expr_rhs): {
    //  use += uses(expr_lhs) + uses(expr_rhs);
    //}
    //case geq(AExpr expr_lhs, AExpr expr_rhs): {
    //  use += uses(expr_lhs) + uses(expr_rhs);
    //}
    //case eq(AExpr expr_lhs, AExpr expr_rhs): {
    //  use += uses(expr_lhs) + uses(expr_rhs);
    //}
    //case neq(AExpr expr_lhs, AExpr expr_rhs): {
    //  use += uses(expr_lhs) + uses(expr_rhs);
    //}
    //case and(AExpr expr_lhs, AExpr expr_rhs): {
    //  use += uses(expr_lhs) + uses(expr_rhs);
    //}
    //case or(AExpr expr_lhs, AExpr expr_rhs): {
    //  use += uses(expr_lhs) + uses(expr_rhs);
    //}
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
    //switch(q){
    //  case nq: normalQuestion(str _, str id, AType _): {
    //    def += {<id, nq.src>};
    //  }
    //  case cq: computedQuestion(str _, str id, AType _, AExpr _): {
    //    def += {<id, cq.src>};
    //  }
    //}
  }
  return def; 
}