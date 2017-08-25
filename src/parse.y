%{
#define YYDEBUG 1
#define YYERROR_VERBOSE 1
/* yydebug = 1; */

#include <stdio.h>

#include "romen.h"
#include "ast.h"
%}

%union {
  ast* node;
  ast_inner_string inner_str;
}

%type <node> program stmts stmt_list stmt
%type <node> expr primary block condition prim_number
%type <node> opt_fargs fargs farg
%type <node> opt_expr_args expr_args
%type <inner_str> identifier var fname cname

%token ident_let ident_fn ident_class
%token op_add op_sub op_mul op_div op_eq op_neq op_assign
%token identifier prim_number

%pure-parser
%parse-param {parse_result* r}
%parse-param {parse_info* p}
%lex-param {p}

%{
int yylex(YYSTYPE*, parse_info*);
static void yyerror(parse_result*, parse_info*, const char* s);
%}

%nonassoc op_eq op_neq op_assign
%left op_add op_sub
%left op_mul op_div

%%
program         : stmts
                    {
                      r->value = $1;
                    }
                ;

stmts           : stmt_list opt_endterms
                    {
                      $$ = $1;
                    }
                | endterms stmt_list opt_endterms
                    {
                      $$ = $2;
                    }
                | opt_endterms
                    {
                      $$ = NULL;
                    }
                ;

stmt_list       : stmt
                    {
                      $$ = new_ast_list();
                      if ($1) {
                        add_ast_list($$, $1);
                      }
                    }
                | stmt_list endterms stmt
                    {
                      $$ = $1;
                      if ($3) {
                        if ($1) {
                          add_ast_list($$, $3);
                        } else {
                          $1 = $3;
                        }
                      }
                    }
                ;

var             : identifier
                ;

fname           : identifier
                ;

cname       : identifier
                ;

stmt            : ident_let var op_assign expr
                    {
                      $$ = new_ast_let($2, $4);
                    }
                | ident_fn fname '(' opt_fargs ')' block
                    {
                      $$ = new_ast_func($2, $4, $6);
                    }
                | ident_class cname block
                    {
                      $$ = new_ast_class($2, $3);
                    }
                | block
                | expr
                ;

block           : '{' stmts '}'
                    {
                      $$ = new_ast_block($2);
                    }
                ;

expr            : expr op_add expr
                    {
                      $$ = new_ast_op("+", $1, $3);
                    }
                | expr op_sub expr
                    {
                      $$ = new_ast_op("-", $1, $3);
                    }
                | expr op_mul expr
                    {
                      $$ = new_ast_op("*", $1, $3);
                    }
                | expr op_div expr
                    {
                      $$ = new_ast_op("/", $1, $3);
                    }
                | expr op_eq expr
                    {
                      $$ = new_ast_op("==", $1, $3);
                    }
                | expr op_neq expr
                    {
                      $$ = new_ast_op("!=", $1, $3);
                    }
                | var op_assign expr
                    {
                      $$ = new_ast_let($1, $3);
                    }
                | fname '(' opt_expr_args ')'
                    {
                      ast* func = new_ast_ident($1);
                      $$ = new_ast_func_call(func, $3);
                    }
                | condition
                | primary
                ;

opt_expr_args   : /* none */
                    {
                      $$ = NULL;
                    }
                | expr_args
                    {
                      $$ = $1;
                    }
                ;

expr_args       : expr
                    {
                      $$ = new_ast_list();
                      $$->type = AST_ARGS;
                      if ($1) {
                        add_ast_list($$, $1);
                      }
                    }
                | expr_args ',' expr
                    {
                      $$ = $1;
                      if ($3) {
                        if ($1) {
                          add_ast_list($$, $3);
                        } else {
                          $1 = $3;
                        }
                      }
                    }
                ;

primary         : prim_number
                | var
                    {
                      $$ = new_ast_ident($1);
                    }
                ;

condition       : '(' expr ')'
                    {
                      $$ = $2;
                    }
                ;

opt_fargs       : /* none */
                    {
                      $$ = NULL;
                    }
                | fargs
                    {
                      $$ = $1;
                    }
                ;

fargs           : farg
                    {
                      $$ = new_ast_list();
                      $$->type = AST_ARGS;
                      if ($1) {
                        add_ast_list($$, $1);
                      }
                    }
                | fargs ',' farg
                    {
                      $$ = $1;
                      if ($3) {
                        if ($1) {
                          add_ast_list($$, $3);
                        } else {
                          $1 = $3;
                        }
                      }
                    }
                ;

farg            : var
                    {
                      $$ = new_ast_ident($1);
                    }

opt_endterms    : /* none */
                | endterms
                ;

endterms        : endterm
                | endterms endterm
                ;

endterm         : ';'
                | '\n'
                ;
%%

#include "lex.yy.c"

static void yyerror(parse_result* r, parse_info* p, const char* s) {
	fprintf(stderr, "yyerror: %s\n", s);
}
