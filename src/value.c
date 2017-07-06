#include "romen.h"

void romen_gc_init() {
	  GC_INIT();
}

void romen_value_init(romen_env* env) {
	return;
}

romen_value* new_rv_number(long long val) {
	romen_value* rn = GC_MALLOC(sizeof(romen_value));
	rn->as.number.val = val;
	return rn;
}
