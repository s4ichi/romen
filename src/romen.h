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

typedef struct parse_result {
	int is_err;
	void* value;
} parse_result;

typedef struct parse_info {
	int lineno;
	exec_mode mode;
} parse_info;

#endif
