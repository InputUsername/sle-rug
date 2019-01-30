module Testing

import Syntax;
import AST;
import CST2AST;
import Check;
import Eval;
import Resolve;
import Compile;
import Transform;

import ParseTree;
import Message;

public loc TAX = |project://QL/examples/tax.myql|;
public loc ERRORS = |project://QL/examples/tax.myql|;

start[Form] testParse(loc file)
  = parse(#start[Form], file);

AForm testAst(loc file)
  = cst2ast(testParse(file));

UseDef testResolve(loc file)
  = resolve(testAst(file));

set[Message] testCheck(loc file) {
  ast = testAst(file);
  env = collect(ast);
  useDef = resolve(ast);
  return check(ast, env, useDef);
}

void testCompile(loc file) {
  compile(testAst(file));
}

AForm testFlatten(loc file)
  = flatten(testAst(file));