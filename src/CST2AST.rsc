module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;
import Boolean;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

//AForm cst2ast(start[Form] sf) {
//  Form f = sf.top; // remove layout before and after form
//  return form("", [], src=f@\loc); 
//}

AForm cst2ast(start[Form] sf) = cst2ast(sf.top);


AForm cst2ast(f:(Form)`form <Identifier x> { <Question* qs> }`)
  = form("<x>", [cst2ast(q) | Question q <- qs]
  , src = f@\loc);

AQuestion cst2ast(Question q) {
  switch (q) {
    case (Question)`<Str label> <Identifier id> : <Type t>`:
      return normalQuestion("<label>"[1..-1], "<id>", cst2ast(t), src = q@\loc);
    
    case (Question)`<Str label> <Identifier id> : <Type t> = <Expr expr>`:
      return computedQuestion("<label>"[1..-1], "<id>", cst2ast(t), cst2ast(expr), src = q@\loc);
      
    case (Question)`{ <Question* qs> }`:
   	  return block([cst2ast(question) | Question question <- qs], src = q@\loc);
   	
   	case (Question)`if ( <Expr expr> ) { <Question* qs> }`:
   	  return if_then(cst2ast(expr), [cst2ast(question) | Question question <- qs], src = q@\loc);
   	
   	case (Question)`if ( <Expr expr> ) { <Question* if_qs> } else { <Question* else_qs> }`:
   	{
   	  aexpr = cst2ast(expr);
   	  if_aqs = [cst2ast(question) | Question question <- if_qs];
   	  else_aqs = [cst2ast(question) | Question question <- else_qs];
   	  return if_then_else(aexpr, if_aqs, else_aqs, src = q@\loc);
   	}
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Identifier x>`: return ref("<x>", src=x@\loc);
    case (Expr)`<Str s>`: return string("<s>", src=s@\loc);
    case (Expr)`<Int i>`: return integer(toInt("<i>"), src=i@\loc);
    case (Expr)`<Bool b>`: return boolean(fromString("<b>"), src=b@\loc);
    case (Expr)`( <Expr expr> )`: return cst2ast(expr);
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
  switch (t) {
    case (Type)`string`: return stringType(src=t@\loc);
    case (Type)`boolean`: return booleanType(src=t@\loc);
    case (Type)`integer`: return integerType(src=t@\loc);
  }
}