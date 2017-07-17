// Romen Environment has key-value data structure
// that implemented by Treap algorithm.

// - Time complexity (n = number of values in treap)
//   - insert cost: O(log n)
//   - erase cost: O(log n)
//   - find cost: O(log n)

#include "romen.h"

Treap* new_treap(const treap_key k, const treap_value v) {
	Treap* t = (Treap*)malloc(sizeof(Treap));

	t->left = NULL;
	t->right = NULL;

	t->priority = rand();
	t->key = k;
	t->value = v;

	return t;
}

Treap* treap_rotate(Treap *t, const int balance) {
	Treap* s;

	if (balance) {
		s = t->left;
		t->left = t->right;
		t->right = t;
	} else {
		s = t->right;
		t->right = t->left;
		t->left = t;
	}

	return s;
}

Treap* treap_find(Treap* t, const treap_key k) {
	if (!t || t->key == k) return t;

	if (strcmp(t->key, k) < 0)
		return treap_find(t->left, k);
	else
		return treap_find(t->right, k);
}

Treap* treap_insert(Treap* t, const treap_key k, const treap_value v) {
	int balance = strcmp(t->key, k);

	if (!t) return new_treap(k, v);
	if (balance == 0) return t;

	if (balance > 0) {
		t->left = treap_insert(t->left, k, v);

		if (t->priority > t->left->priority)
			treap_rotate(t, 1);
	} else {
		t->right = treap_insert(t->right, k, v);

		if (t->priority > t->right->priority)
			treap_rotate(t, 0);
	}

	return t;
}


Treap* treap_erase(Treap* t, const treap_key k) {
	int balance = strcmp(t->key, k);

	if (!t) return NULL;

	if (balance > 0) {
		t->left = treap_erase(t->left, k);
		return t;
	} else {
		t->right = treap_erase(t->right, k);
		return t;
	}

	if (!t->left && !t->right)
		return NULL;

	if (!t->left)
		t = treap_rotate(t, 0);
	else if (!t->right)
		t = treap_rotate(t, 1);
	else {
		if (t->left->priority < t->right->priority)
			t = treap_rotate(t, 0);
		else
			t = treap_rotate(t, 1);

		t = treap_erase(t, k);
	}

	return t;
}
