#include <stdio.h>
#include <time.h>
#include "romen.h"

void romen_env_init(romen_env* env) {

#ifdef DEBUG
	srand(1);
#else
	srand((unsigned)time(NULL));
#endif

	env->as = NULL;
	env->next = NULL;

	return;
}

romen_env* new_env() {
	romen_env* env = (romen_env*)malloc(sizeof(romen_env));

	// lazy allocate treap data structure
	env->as = NULL;
	env->next = NULL;

	return env;
}

romen_env* env_add(romen_env* env, const env_key k, const env_value v) {
	env->as = treap_insert(env->as, k, v);
	return env;
}

romen_env* env_delete(romen_env* env, const env_key k) {
	env->as = treap_erase(env->as, k);

	return env;
}

env_value env_find(romen_env* env, const env_key k) {
	env_value val = NULL;

	while (!val && env) {
		val = (env_value)treap_find(env->as, k);
		env = env->next;
	}

	return val;
}

env_value env_find_by_current(romen_env* env, const env_key k) {
	return (env_value)treap_find(env->as, k);
}

romen_env* env_pop(romen_env* env) {
	if (!env) return env;

	romen_env* head = env->next;
	free(env);

	return head;
}

romen_env* env_push(romen_env* env, romen_env* t) {
	if (!env || !t) return env;

	t->next = env;
	env = t;

	return env;
}

void fmt_env(romen_env* env, int depth) {
	if (!env) return;

	fmt_treap(env->as, depth);

	if (env->next)
		fprintf(stdout, "\n");

	fmt_env(env->next, depth);
}
