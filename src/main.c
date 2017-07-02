#include <stdio.h>

#include "romen.h"
#include "ast.h"

#include "y.tab.h"
#include "lex.yy.h"

int yyparse(parse_result*, parse_info*);

static void usage(int exit_code) {
	fprintf(exit_code ? stderr : stdout,
		"Usage: romen [ -h ][ -i ][ -e <input_file> ][ -v ]\n\n"
		"  -e input_file     Eval romen file\n"
		"  -v                Print parse experession\n"
		"  -i                Launch interpreter of romen\n"
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
	int res;

	yy_scan_string(s);
	res = yyparse(r, p);

	if (res == 1 || r->is_err == 1) {
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
		fprintf(stdout, "NULL");
		return;
	}

	switch (top_stmt->type) {
	case AST_NUMBER:
		fprintf(stdout, "AST_NUMBER(%lld)\n", ((ast_number*)top_stmt)->value);
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
	parse_result r;
	parse_info p;

	int verbose = 0;
	exec_mode mode = MODE_FILE;

	for (;;) {
		int opt = getopt(argc, argv, "e:hiv");
		if (opt == -1) break;
		switch (opt) {
		case 'v':
			verbose = 1;
			break;
		case 'e':
			mode = MODE_EVAL;
			parse_from_string(optarg, &r, &p);
			break;
		case 'i':
			mode = MODE_REPL;
			// TODO
			usage(0);
			break;
		case 'h':
			usage(0);
			break;
		default:
			usage(1);
		}
	}

	if (mode == MODE_FILE) {
		// TODO
		// open file and parse
	}

	if (verbose) {
		fmt_ast(r.value, 0);
	} else {
		// TODO
		// eval to e.value
	}

	return 0;
}
