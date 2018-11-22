module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Id "{" Question* "}"; 

// TODO: question, computed question, block, if-then-else, if-then
syntax Question
  = 
  ; 

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr  //TODO: This still doesn't function properly.
  = Id \ "true" \ "false" // true/false are reserved keywords.
  | Str
  | Int
  | Bool
  | "(" Expr ")"
  | "!" Expr
  > left Expr "*" Expr !>> "*" Expr
  | left Expr "/" Expr !>> "/" Expr
  > left Expr "+" Expr
  | left Expr "-" Expr
  > non-assoc Expr "\>" Expr
  | non-assoc Expr "\<" Expr
  | non-assoc Expr "\<=" Expr
  | non-assoc Expr "\>=" Expr
  > left Expr "==" Expr
  | left Expr "!=" Expr
  > left Expr "&&" Expr
  > left Expr "||" Expr;
  
syntax Type
  = "string" 
  | "integer" 
  | "boolean";  
  
lexical Str = "\"" 
		      (![\"]|"\\\"")* //Accept anything that isn't a quote; support escape characters. 
		      "\"";

lexical Int 
  = [+\-]? [0]
  | [+\-]? [1-9][0-9]*
  !>> [0-9];

lexical Bool = "true" | "false";



