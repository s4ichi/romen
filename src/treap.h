#ifndef TREAP_H
#define TREAP_H

#include "romen.h"

typedef env_key treap_key;
typedef env_value treap_value;

typedef struct Treap {
	int priority;

	treap_key key;
	treap_value value;

	struct Treap* left;
	struct Treap* right;
} Treap;

extern Treap* new_treap(const treap_key, const treap_value);
extern Treap* rotate(Treap*, const int);
extern Treap* find(Treap*, const treap_key);
extern Treap* insert(Treap*, const treap_key, const treap_value);
extern Treap* erase(Treap*, const treap_key);

#endif // TREAP_H
