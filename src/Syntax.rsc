module Syntax

extend lang::std::Id;
extend lang::std::Layout;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Identifier Block; 

// TODO: question, computed question, block, if-then-else, if-then
// We chose to force the use of brackets in if-statements, to not worry about a trailing else.
syntax Question
  = 
  Str Identifier ":" Type //normal question
  | Str Identifier ":" Type "=" Expr //computed question
  | Block //block
  | "if" "(" Expr ")" Block !>> "else" //if/then
  | "if" "(" Expr ")" Block "else" Block //if/then/else
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

syntax Identifier = Id \ Reserved;  // true/false are reserved keywords.

syntax Type
  = "string" 
  | "integer" 
  | "boolean";  
  
lexical Str = "\"" 
		      (![\"]|"\\\"")* // accept anything that isn't a quote, plus escaped quotes
		      "\"";

lexical Int 
  = [+\-]? [0]
  | [+\-]? [1-9][0-9]*
  !>> [0-9];

lexical Bool = "true" | "false";

keyword Reserved
  = "true"
  | "false"
  | "form"
  | "if"
  | "else";


