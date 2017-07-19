#include <stdio.h>
#include "romen.h"

void binding_check(romen_env* env, ast* top_stmt, int depth) {
	int i;
	for (i = 0; i < depth; i++) {
		putchar('\t');
	}

	if (!top_stmt) {
		fprintf(stdout, "NULL\n");
		return;
	}

	switch (top_stmt->type) {
	case AST_NUMBER:
		fprintf(stdout, "AST_NUMBER(%lld)\n", ((ast_number*)top_stmt)->value);
		break;
	case AST_IDENT:
		fprintf(stdout, "AST_IDENT(%s)\n", ((ast_ident*)top_stmt)->name->buffer);

		if (!env_find(env, ((ast_ident*)top_stmt)->name->buffer)) {
			fprintf(stderr, "Error: Not found variable %s\n", ((ast_ident*)top_stmt)->name->buffer);
			exit(1);
		}

		break;
	case AST_OP:
		fprintf(stdout, "AST_OP(%s):\n", ((ast_op*)top_stmt)->op->buffer);
		binding_check(env, ((ast_op*)top_stmt)->lhs, depth + 1);
		binding_check(env, ((ast_op*)top_stmt)->rhs, depth + 1);
		break;
	case AST_LET:
		fprintf(stdout, "AST_LET(%s):\n", ((ast_let*)top_stmt)->lhs->buffer);
		binding_check(env, ((ast_let*)top_stmt)->rhs, depth + 1);
		env = env_add(env, ((ast_let*)top_stmt)->lhs->buffer, new_rv_number(0));
		break;
	case AST_FUNC_CALL:
		fprintf(stdout, "AST_FUNC_CALL(%s):\n", ((ast_ident*)((ast_func_call*)top_stmt)->func)->name->buffer);
		binding_check(env, ((ast_func_call*)top_stmt)->args, depth + 1);
		break;
	case AST_BLOCK:
		env = env_push(env, (romen_env*)new_env());
		fprintf(stdout, "AST_BLOCK:\n");
		binding_check(env, ((ast_block*)top_stmt)->body, depth + 1);

		for (i = 0; i < depth + 1; i++)
			fprintf(stdout, "\t");
		fprintf(stdout, "ENV:\n");

		fmt_env(env, depth + 2);
		env = env_pop(env);
		break;
	case AST_FUNC:
		fprintf(stdout, "AST_FUNC(%s):\n", ((ast_func*)top_stmt)->name->buffer);

		env = env_add(env, ((ast_func*)top_stmt)->name->buffer, new_rv_number(1));

		env = env_push(env, (romen_env*)new_env());

		ast_list* arg_list = (ast_list*)((ast_func*)top_stmt)->args;
		for (i = 0; i < arg_list->size; i++) {
			if (arg_list->stmts[i]->type != AST_IDENT) {
				fprintf(stderr, "Error: function arg is not AST_IDENT\n");
				exit(1);
			}

			env = env_add(env, ((ast_ident*)arg_list->stmts[i])->name->buffer, new_rv_number(0));
		}

		binding_check(env, ((ast_block*)(((ast_func*)top_stmt)->body))->body, depth + 1);

		for (i = 0; i < depth + 1; i++)
			fprintf(stdout, "\t");
		fprintf(stdout, "ENV:\n");

		fmt_env(env, depth + 2);
		env = env_pop(env);

		break;
	case AST_ARGS:
		fprintf(stdout, "AST_ARGS:\n");
		for (i = 0; i < ((ast_list*)top_stmt)->size; i++)
			binding_check(env, ((ast_list*)top_stmt)->stmts[i], depth + 1);
		break;
	case AST_LIST:
		fprintf(stdout, "AST_LIST:\n");
		for (i = 0; i < ((ast_list*)top_stmt)->size; i++)
			binding_check(env, ((ast_list*)top_stmt)->stmts[i], depth + 1);
		break;
	default:
		fprintf(stdout, "NO SUCH TYPE\n");
		break;
	}
}
