#ifndef ROMEN_H
#define ROMEN_H

typedef enum {
	MODE_REPL,
	MODE_FILE,
	MODE_EVAL,
} exec_mode;

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>

#include <gc/gc.h>

// ----- Parser, Lexer Definitions -----

typedef struct parse_result {
	int is_err;
	void* value;
} parse_result;

typedef struct parse_info {
	int lineno;
	exec_mode mode;
} parse_info;
extern void romen_parse_init(parse_result*, parse_info*m, exec_mode);
extern void parse_from_string(const char*, parse_result*m, parse_info*);
extern void parse_from_file(const char*, parse_result*m, parse_info*);

// ----- /Parser, Lexer Definitions -----

#endif
