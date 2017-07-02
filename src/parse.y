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
%type <inner_str> identifier var

%token ident_let
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

stmt            : ident_let var op_assign expr
                    {
                      $$ = new_ast_let($2, $4);
                    }
                | expr
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
                | primary
                ;

primary         : prim_number
                | var
                    {
                      $$ = new_ast_ident($1);
                    }
                | condition
                | block
                ;

block           : '{' stmts '}'
                    {
                      $$ = new_ast_block($2);
                    }

condition       : '(' expr ')'
                    {
                      $$ = $2;
                    }
                ;

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
