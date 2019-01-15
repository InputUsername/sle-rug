module Transform

import Syntax;
import Resolve;
import AST;

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (a && b) q1: "" int;
 *     if (a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 * 
 * Note to self:
 *     if (a) {
 *       q0: "" int;
 *     } else {
 *       q1: "" int;
 *     }
 *
 *  is equivalent to
 *     if (a)  q0: "" int;
 *     if (!a) q1: "" int;
 */
 
/* Note: doesn't resolve conditionals for flattening,
 *       just filters out the original environment conditional of "true". 
 */

AExpr join_cond_exprs(AExpr expr1, AExpr expr2)
 = and(expr1, expr2);

AExpr flatten_cond_expr(and(boolean(true), AExpr expr)) = expr;
AExpr flatten_cond_expr(and(AExpr expr, boolean(true))) = expr;

/* Not sure whether the following are required,
 but implemented them for completeness' sake. */
AExpr flatten_cond_expr(or(boolean(false), AExpr expr)) = expr;
AExpr flatten_cond_expr(or(AExpr expr, boolean(false))) = expr;

AExpr flatten_cond_expr(AExpr expr) = expr; //base case


AExpr join_and_flatten_cond_exprs(AExpr expr1, AExpr expr2)
 = flatten_cond_expr(join_cond_exprs(expr1, expr2));


list[AQuestion] flatten_rec(normalQuestion(str label, str id, AType t), AExpr env_conditional)
 = [ if_then(env_conditional, [ normalQuestion(label, id, t) ]) ];

list[AQuestion] flatten_rec(computedQuestion(str label, str id, AType t, AExpr expr), AExpr env_conditional)
 = [ if_then(env_conditional, [ computedQuestion(label, id, t, expr) ]) ];

list[AQuestion] flatten_rec(block(list[AQuestion] qs), AExpr env_conditional)
 = ( [] | it + flatten_rec(q, env_conditional) | q <- qs );

list[AQuestion] flatten_rec(if_then(AExpr expr, list[AQuestion] qs), AExpr env_conditional)
 = ( [] | it + flatten_rec(q, join_and_flatten_cond_exprs(env_conditional, expr)) | q <- qs );

list[AQuestion] flatten_rec(if_then_else(AExpr expr, list[AQuestion] if_qs, list[AQuestion] else_qs), AExpr env_conditional)
 = ( [] | it + flatten_rec(q, join_and_flatten_cond_exprs(env_conditional, expr)) | q <- if_qs )
 + ( [] | it + flatten_rec(q, join_and_flatten_cond_exprs(env_conditional, not(expr))) | q <- else_qs );


AForm flatten(form(str name, list[AQuestion] questions))
 = form(name, ( [] | it + flatten_rec(q, boolean(true)) | q <- questions ));

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */

start[Form] rename(start[Form] f, loc useOrDef, str newName, UseDef useDef) {
  AForm af = parse(f);
  Use use = uses(af);
  Def def = defs(af);
  
  eqClass = {};
  
  for (/Question q <- f.questions) {
    ;
  }
}

value resolveTest(UseDef useDef) {
  u = { u | <u, useOrDef> <- useDef };
  d = { d | <useOrDef, d> <- useDef };
  
  return <u, d>;
}