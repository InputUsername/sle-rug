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
      return normalQuestion(label, "<id>", cst2ast(t));
    
    case (Question)`<Str label> <Identifier id> : <Type t> = <Expr expr>`:
      return computedQuestion(label, "<id>", cst2ast(t), cst2ast(expr));
      
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
    case (Expr)`<Id x>`: return ref("<x>", src=x@\loc);
    
    // etc.
    
    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(Type t) {
  switch (t) {
    case (Type)`string`: return stringType();
    case (Type)`boolean`: return booleanType();
    case (Type)`integer`: return integerType();
  }
}
