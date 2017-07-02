NUMBER ([1-9][0-9]*|0)
CHAR   [a-zA-Z_]
REST   ({CHAR}|[0-9])
IDENT  {CHAR}{REST}*

%{
#include <string.h>

#include "romen.h"
#include "ast.h"

#define YY_DECL int yylex(YYSTYPE* yylval, parse_info* p)
%}

%option noyywrap
%option yylineno

%%
"+"           { return op_add; };
"-"           { return op_sub; };
"*"           { return op_mul; };
"/"           { return op_div; };
"=="          { return op_eq; };
"!="          { return op_neq; };
"="           { return op_assign; };
"\n"          { return '\n'; };
";"           { return ';'; };
"#"[^\n]*"\n" { return '\n'; };
[(){}@]       { return yytext[0]; };

let { return ident_let; };

{IDENT} {
        yylval->inner_str = new_ast_inner_string(yytext, yyleng);
        return identifier;
};

{NUMBER} {
        long long temp;
        sscanf(yytext, "%lld", &temp);
        yylval->node = (ast*)new_ast_number(temp);
        return prim_number;
};

[ \t]  ;

. {
        fprintf(stderr, "lexical error, line: %d\n", yylineno);
        exit(1);
};
%%
