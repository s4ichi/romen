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

// Use boehmGC
// Also see http://www.hboehm.info/gc/gcinterface.html
#include <gc/gc.h>

// ----- Parser, Lexer Definitions ----- //

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

// ----- /Parser, Lexer Definitions ----- //

// ---- Value Definitions ----- //

typedef struct rv_number {
	long long val;
} rv_number;

typedef struct romen_value {
	union {
		struct rv_number number;
	} as;
} romen_value;

// ---- Environment Definitions ----- //

typedef char* env_key;
typedef romen_value* env_value;

typedef struct romen_env {

} romen_env;

// ---- Value Functions ----- //

extern void romen_gc_init();
extern void romen_value_init(romen_env*);
extern romen_value* new_rv_number(long long);

// ---- Environment Functions ----- //

extern void romen_env_init(romen_env*);


#endif
