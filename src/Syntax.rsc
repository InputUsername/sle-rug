module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Identifier Block; 

// TODO: question, computed question, block, if-then-else, if-then
// We chose to force the use of brackets in if-statements, to not worry about a trailing else.
syntax Question
  = 
  normalQuestion: Str Identifier ":" Type
  | computedQuestion: Str Identifier ":" Type "=" Expr
  | block: Block
  | if_then: "if" "(" Expr ")" Block !>> "else"
  | if_then_else: "if" "(" Expr ")" Block "else" Block
  ;

syntax Block = "{" Question* "}";

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr
  = Identifier
  | Str
  | Int
  | Bool
  | "(" Expr ")"
  | "!" Expr
  > left (
  	Expr "*" Expr
  	| Expr "/" Expr
  )
  > left (
  	Expr "+" Expr
  	| Expr "-" Expr
  )
  > non-assoc (
  	Expr "\>" Expr
  	| Expr "\<" Expr
  	| Expr "\<=" Expr
  	| Expr "\>=" Expr
  )
  > left (
  	Expr "==" Expr
  	| Expr "!=" Expr
  )
  > left Expr "&&" Expr
  > left Expr "||" Expr;

syntax Identifier = Id \"true" \"false";  // true/false are reserved keywords.

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



