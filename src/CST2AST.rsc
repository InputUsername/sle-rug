module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) {
  Form f = sf.top; // remove layout before and after form
  return form("", [], src=f@\loc); 
}

AQuestion cst2ast(Question q) {
  throw "Not yet implemented";
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: return ref("<x>", src=x@\loc);
    case (Expr)`<Str s>`: return string("<s>", src=s@\loc);
    case (Expr)`<Int i>`: return integer(toInt("<i>"), src=i@\loc);
    case (Expr)`( <Expr expr> )`: return cst2ast(e);
    case (Expr)`! <Expr expr>`: return not(cst2ast(e), src=e@\loc);
    case (Expr)`<Expr lhs> * <Expr rhs>`: return mul(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> / <Expr rhs>`: return div(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> + <Expr rhs>`: return add(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> - <Expr rhs>`: return sub(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> \> <Expr rhs>`: return gt(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> \< <Expr rhs>`: return lt(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> \<= <Expr rhs>`: return leq(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> == <Expr rhs>`: return eq(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> != <Expr rhs>`: return neq(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> && <Expr rhs>`: return and(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> || <Expr rhs>`: return or(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    
    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(Type t) {
  throw "Not yet implemented";
}