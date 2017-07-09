// Romen Environment has key-value data structure
// that implemented by Treap algorithm.

// - Time complexity (n = number of values in treap)
//   - insert cost: O(log n)
//   - erase cost: O(log n)
//   - find cost: O(log n)

#include "treap.h"

Treap* new_treap(const treap_key k, const treap_value v) {
	Treap* t = (Treap*)malloc(sizeof(Treap));

	t->left = NULL;
	t->right = NULL;

	t->priority = rand();
	t->key = k;
	t->value = v;

	return t;
}

Treap* rotate(Treap *t, const int balance) {
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

Treap* find(Treap* t, const treap_key k) {
	if (!t || t->key == k) return t;

	if (strcmp(t->key, k) < 0)
		return find(t->left, k);
	else
		return find(t->right, k);

}

Treap* insert(Treap* t, const treap_key k, const treap_value v) {
	int balance = strcmp(t->key, k);

	if (!t) return new_treap(k, v);
	if (balance == 0) return t;

	if (balance > 0) {
		t->left = insert(t->left, k, v);

		if (t->priority > t->left->priority)
			rotate(t, 1);
	} else {
		t->right = insert(t->right, k, v);

		if (t->priority > t->right->priority)
			rotate(t, 0);
	}

	return t;
}


Treap* erase(Treap* t, const treap_key k) {
	int balance = strcmp(t->key, k);

	if (!t) return NULL;

	if (balance > 0) {
		t->left = erase(t->left, k);
		return t;
	} else {
		t->right = erase(t->right, k);
		return t;
	}

	if (!t->left && !t->right)
		return NULL;

	if (!t->left)
		t = rotate(t, 0);
	else if (!t->right)
		t = rotate(t, 1);
	else {
		if (t->left->priority < t->right->priority)
			t = rotate(t, 0);
		else
			t = rotate(t, 1);

		t = erase(t, k);
	}

	return t;
}
