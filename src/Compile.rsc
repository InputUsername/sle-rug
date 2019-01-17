module Compile

import AST;
import Resolve;
import IO;
import lang::html5::DOM; // see standard library

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTML5Node type and the `str toString(HTML5Node x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */
 
void compile(AForm f) {
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, toString(form2html(f)));
}

/* Convert a list of questions to HTML by converting the questions
 * to HTML and putting them in a div.
 */
HTML5Node questionlist2html(list[AQuestion] qs) {
  HTML5Node block = div();
  for (AQuestion q <- qs) {
    block.kids += [ question2html(q) ];
  }
  return block;
}

// Return a unique identifier for if statements
str if2identifier(loc src)
  = "conditions$if_<src.begin.line>_<src.begin.column>";

/******************* form2html *******************/

HTML5Node form2html(AForm f)
  = html(
      head(title("Questionnaire")),
      body(
        div(
          id("app"),
          questionlist2html(f.questions)
        ),
        script(src("https://cdn.jsdelivr.net/npm/vue@2.5.21/dist/vue.min.js")),
        script(src(f.src[extension="js"].file))
      )
    );

// Generate inputs for string, boolean and integer questions

HTML5Node questionInput(str questionId, stringType())
  = input(html5attr("v-model", questionId), \type("text"));

HTML5Node questionInput(str questionId, booleanType())
  = input(html5attr("v-model", questionId), \type("checkbox"));

HTML5Node questionInput(str questionId, integerType())
  = input(html5attr("v-model.number", questionId), \type("number"));

// Generate HTML for different question types

HTML5Node question2html(normalQuestion(str label, str questionId, AType t))
  = div(p(label), questionInput(questionId, t));

HTML5Node question2html(computedQuestion(str label, str questionId, AType t, AExpr _)) {
  HTML5Node input = questionInput(questionId, t);
  input.kids += [ readonly("true") ];
  
  return div(p(label), input);
}

HTML5Node question2html(block(list[AQuestion] qs))
  = questionlist2html(qs);

HTML5Node question2html(ifThen(AExpr _, list[AQuestion] qs, src=loc u)) {
  HTML5Node questionsDiv = questionlist2html(qs);
  questionsDiv.kids += [ html5attr("v-if", if2identifier(u)) ];
  
  return questionsDiv;
}

HTML5Node question2html(ifThenElse(AExpr _, list[AQuestion] ifQuestions, list[AQuestion] elseQuestions, src=loc u)) {
  HTML5Node ifQuestionsDiv = questionlist2html(ifQuestions);
  HTML5Node elseQuestionsDiv = questionlist2html(elseQuestions);
  ifQuestionsDiv.kids += [ html5attr("v-if", if2identifier(u)) ];
  elseQuestionsDiv.kids += [ html5attr("v-else", "") ];
  
  return div(ifQuestionsDiv, elseQuestionsDiv);
}

/******************* form2js *******************/

// Translate abstract expressions to JavaScript

str aExpr2js(ref(str name))
  = "this.<name>";

str aExpr2js(string(str s))
  = "\"<s>\"";
 
str aExpr2js(integer(int i))
  = "<i>";

str aExpr2js(boolean(bool b))
  = "<b>";

str aExpr2js(not(AExpr expr))
  = "!<aExpr2js(expr)>";

str aExpr2js(mul(AExpr expr_lhs, AExpr expr_rhs))
  = "(<aExpr2js(expr_lhs)> * <aExpr2js(expr_rhs)>)";

str aExpr2js(div(AExpr expr_lhs, AExpr expr_rhs))
  = "(<aExpr2js(expr_lhs)> / <aExpr2js(expr_rhs)>)";
 
str aExpr2js(add(AExpr expr_lhs, AExpr expr_rhs))
  = "(<aExpr2js(expr_lhs)> + <aExpr2js(expr_rhs)>)";

str aExpr2js(sub(AExpr expr_lhs, AExpr expr_rhs))
  = "(<aExpr2js(expr_lhs)> - <aExpr2js(expr_rhs)>)";

str aExpr2js(lt(AExpr expr_lhs, AExpr expr_rhs))
  = "(<aExpr2js(expr_lhs)> \< <aExpr2js(expr_rhs)>)";

str aExpr2js(gt(AExpr expr_lhs, AExpr expr_rhs))
  = "(<aExpr2js(expr_lhs)> \> <aExpr2js(expr_rhs)>)";

str aExpr2js(leq(AExpr expr_lhs, AExpr expr_rhs))
  = "(<aExpr2js(expr_lhs)> \<= <aExpr2js(expr_rhs)>)";

str aExpr2js(geq(AExpr expr_lhs, AExpr expr_rhs))
  = "(<aExpr2js(expr_lhs)> \>= <aExpr2js(expr_rhs)>)";

str aExpr2js(eq(AExpr expr_lhs, AExpr expr_rhs))
  = "(<aExpr2js(expr_lhs)> == <aExpr2js(expr_rhs)>)";

str aExpr2js(neq(AExpr expr_lhs, AExpr expr_rhs))
  = "(<aExpr2js(expr_lhs)> !== <aExpr2js(expr_rhs)>)";

str aExpr2js(and(AExpr expr_lhs, AExpr expr_rhs))
  = "(<aExpr2js(expr_lhs)> && <aExpr2js(expr_rhs)>)";

str aExpr2js(or(AExpr expr_lhs, AExpr expr_rhs))
  = "(<aExpr2js(expr_lhs)> || <aExpr2js(expr_rhs)>)";

// Default values in JavaScript for question types
str defaultValue(stringType()) = "\"\"";
str defaultValue(booleanType()) = "false";
str defaultValue(integerType()) = "0";

/*
 * Generate JavaScript for a form by filling in a template.
 * We chose to use Vue.js, which supports plain data bindings
 * (in the data object of the Vue constructor) as well as
 * computed values (in the computed object of the constructor).
 * Normal questions will be translated to plain data values,
 * whereas the expressions of computed questions and if statements
 * will be computed.
 */
str form2js(AForm f)
  = "var app = new Vue({
    '  el: \"#app\",
    '  data: {
    '    <for (/normalQuestion(str label, str questionId, AType t) := f.questions) {>
    '    <questionId>: <defaultValue(t)>,
    '    <}>
    '  },
    '  computed: {
    '    <for (/computedQuestion(str _, str questionId, AType _, AExpr expr) := f.questions) {>
    '    <questionId>: function() {
    '      return <aExpr2js(expr)>;
    '    },
    '    <}>
    '    <for (/ifThen(AExpr expr, list[AQuestion] _, src=loc u) := f.questions) {>
    '    <if2identifier(u)>: function() {
    '      return <aExpr2js(expr)>;
    '    },
    '    <}>
    '    <for (/ifThenElse(AExpr expr, list[AQuestion] _, list[AQuestion] _, src=loc u) := f.questions) {>
    '    <if2identifier(u)>: function() {
    '      return <aExpr2js(expr)>;
    '    },
    '    <}>
    '  },
    '});";
