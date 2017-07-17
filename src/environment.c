#include <time.h>
#include "romen.h"

void romen_env_init(romen_env* env) {
	srand((unsigned)time(NULL));
	return;
}

romen_env* new_env() {
	romen_env* env = (romen_env*)malloc(sizeof(romen_env));

	// lazy allocate treap data structure
	env->as = NULL;
	env->next = NULL;

	return env;
}

void env_add(romen_env* env, const env_key k, const env_value v) {
	if (env->as == NULL) {
		env->as = new_treap(k, v);
	} else {
		treap_insert(env->as, k, v);
	}
}

void env_delete(romen_env* env, const env_key k) {
	treap_erase(env->as, k);
}

env_value env_find(romen_env* env, const env_key k) {
	env_value val = NULL;

	while (!val && env) {
		val = (env_value)treap_find(env->as, k);
		env = env->next;
	}

	return val;
}

env_value env_find_by_curret(romen_env* env, const env_key k) {
	return (env_value)treap_find(env->as, k);
}
