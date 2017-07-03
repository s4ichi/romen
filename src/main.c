#include <stdio.h>

#include "romen.h"
#include "ast.h"

#include "y.tab.h"
#include "lex.yy.h"

int yyparse(parse_result*, parse_info*);

static void usage(int exit_code) {
	fprintf(exit_code ? stderr : stdout,
		"Usage: romen [ -h ][ -e <input_file> ][ -v ]\n\n"
		"  -e input_file     Eval romen file\n"
		"  -v                Print parse experession\n"
		"  -h                Print this help\n"
		"\n"
		"One of -e or -v must be specified.\n\n");

	exit(exit_code);
}


void romen_parse_init(parse_result* r, parse_info* p, exec_mode m) {
	r->is_err = 0;
	r->value = NULL;

	p->lineno = 0;
	p->mode = m;
}

void parse_from_string(const char* s, parse_result* r, parse_info* p) {
	int err;

	yy_scan_string(s);
	err = yyparse(r, p);

	if (err == 1 || r->is_err == 1) {
		fprintf(stderr, "Parse Error\n");
		exit(1);
	}
}

void parse_from_file(const char* path, parse_result* r, parse_info* p) {
	int err;

	FILE* fp = fopen(path, "rb");
	if (!fp) {
		fprintf(stderr, "File open error\n");
		exit(1);
	}

	yyrestart(fp);
	err = yyparse(r, p);

	if (err == 1 || r->is_err == 1) {
		fprintf(stderr, "Parse Error\n");
		exit(1);
	}
}

void fmt_ast(ast* top_stmt, int depth) {
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
		break;
	case AST_OP:
		fprintf(stdout, "AST_OP(%s):\n", ((ast_op*)top_stmt)->op->buffer);
		fmt_ast(((ast_op*)top_stmt)->lhs, depth + 1);
		fmt_ast(((ast_op*)top_stmt)->rhs, depth + 1);
		break;
	case AST_LET:
		fprintf(stdout, "AST_LET(%s):\n", ((ast_let*)top_stmt)->lhs->buffer);
		fmt_ast(((ast_let*)top_stmt)->rhs, depth + 1);
		break;
	case AST_BLOCK:
		fprintf(stdout, "AST_BLOCK:\n");
		fmt_ast(((ast_block*)top_stmt)->body, depth + 1);
		break;
	case AST_FUNC:
		fprintf(stdout, "AST_FUNC(%s):\n", ((ast_func*)top_stmt)->name->buffer);
		fmt_ast(((ast_func*)top_stmt)->args, depth + 1);
		fmt_ast(((ast_func*)top_stmt)->body, depth + 1);
		break;
	case AST_ARGS:
		fprintf(stdout, "AST_ARGS:\n");
		for (i = 0; i < ((ast_list*)top_stmt)->size; i++)
			fmt_ast(((ast_list*)top_stmt)->stmts[i], depth + 1);
		break;
	case AST_LIST:
		fprintf(stdout, "AST_LIST:\n");
		for (i = 0; i < ((ast_list*)top_stmt)->size; i++)
			fmt_ast(((ast_list*)top_stmt)->stmts[i], depth + 1);
		break;
	default:
		fprintf(stdout, "NO SUCH TYPE\n");
		break;
	}
}

int main(int argc, char** argv) {
	int verbose = 0;
	char* source = NULL;
	exec_mode mode = MODE_FILE;

	parse_result r;
	parse_info p;

	for (;;) {
		int opt = getopt(argc, argv, "e:hiv");
		if (opt == -1) break;
		switch (opt) {
		case 'v':
			verbose = 1;
			break;
		case 'e':
			mode = MODE_EVAL;
			source = optarg;
			break;
		case 'h':
			usage(0);
			break;
		default:
			usage(1);
		}
	}

	romen_parse_init(&r, &p, mode);

	if (mode == MODE_FILE && optind < argc) {
		source = argv[optind];
		parse_from_file(source, &r, &p);
	} else if (mode == MODE_EVAL && source) {
		parse_from_string(source, &r, &p);
	} else {
		fprintf(stderr, "No such file or source code");
		exit(1);
	}

	if (verbose) fmt_ast(r.value, 0);

	// TODO
	// eval to e.value

	ast_free(r.value);

	return 0;
}
