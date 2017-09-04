#include "romen.h"
#include "ast.h"

ast_inner_string
new_ast_inner_string(const char* name, unsigned int len) {
	ast_inner_string str;

	str = malloc(sizeof(struct ast_inner_string) + len + 1);
	str->len = len;
	memcpy(str->buffer, name, len);
	str->buffer[len] = '\0';

	return str;
}

ast*
new_ast_number(long long i) {
	ast_number* ai = malloc(sizeof(ast_number));

	ai->type = AST_NUMBER;
	ai->value = i;

	return (ast*)ai;
}

ast*
new_ast_ident(ast_inner_string name) {
	ast_ident* ai = malloc(sizeof(ast_ident));

	ai->type = AST_IDENT;
	ai->name = name;

	return (ast*)ai;
}

ast*
new_ast_list(void) {
	ast_list* al = malloc(sizeof(ast_list));

	al->type = AST_LIST;
	al->size = 0;
	al->stmts = NULL;

	return (ast*)al;
}

void
add_ast_list(ast* parent, ast* child) {
	ast_list* p = (ast_list*)parent;

	// realloc every time
	p->stmts = realloc(p->stmts, sizeof(ast*) * (p->size + 1));
	p->stmts[p->size] = child;
	p->size++;
}

ast*
new_ast_class(ast_inner_string cname, ast* body) {
	ast_class* ac = malloc(sizeof(ast_class));

	ac->type = AST_CLASS;
	ac->name = cname;
	ac->body = body;

	return (ast*)ac;
}

ast*
new_ast_module(ast_inner_string mname, ast* body) {
	ast_module* am = malloc(sizeof(ast_module));

	am->type = AST_MODULE;
	am->name = mname;
	am->body = body;

	return (ast*)am;
}

ast*
new_ast_let(ast_inner_string lhs, ast* rhs) {
	ast_let* al = malloc(sizeof(ast_let));

	al->type = AST_LET;
	al->lhs = lhs;
	al->rhs = rhs;

	return (ast*)al;
}

ast*
new_ast_op(const char* op, ast* lhs, ast* rhs) {
	ast_op* aop = malloc(sizeof(ast_op));

	aop->type = AST_OP;
	aop->op = new_ast_inner_string(op, strlen(op));
	aop->lhs = lhs;
	aop->rhs = rhs;

	return (ast*)aop;
}

ast*
new_ast_block(ast* ast_list) {
	ast_block* ab = malloc(sizeof(ast_block));

	ab->type = AST_BLOCK;
	ab->body = ast_list;

	return (ast*)ab;
}

ast*
new_ast_func(ast_inner_string fname, ast* args, ast* body) {
	ast_func* af = malloc(sizeof(ast_func));

	af->type = AST_FUNC;
	af->name = fname;
	af->args = args;
	af->body = body;

	return (ast*)af;
}

ast*
new_ast_func_call(ast* func, ast* args) {
	ast_func_call* afc = malloc(sizeof(ast_func_call));

	afc->type = AST_FUNC_CALL;
	afc->func = func;
	afc->args = args;

	return (ast*)afc;
}

void
ast_free(ast* a) {
	int i;

	if (!a) return;

	switch (a->type) {
	case AST_NUMBER:
		free(a);
		break;
	case AST_OP:
		ast_free(((ast_op*)a)->lhs);
		ast_free(((ast_op*)a)->rhs);
		break;
	case AST_LET:
		ast_free(((ast_let*)a)->rhs);
		break;
	case AST_BLOCK:
		ast_free(((ast_block*)a)->body);
		break;
	case AST_LIST:
		for(i = 0; i < ((ast_list*)a)->size; i++)
			ast_free(((ast_list*)a)->stmts[i]);
		free(((ast_list*)a)->stmts);
		free(a);
		break;
	default:
		break;
	}
}
