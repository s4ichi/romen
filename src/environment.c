#include <time.h>
#include "romen.h"
#include "treap.h"

void romen_env_init(romen_env* env) {
	srand((unsigned)time(NULL));
	return;
}
