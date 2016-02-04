#include "test.h"

int main(void) {
	int x;

#ifdef __FOO__
	x = 1;
#else
	x = 2;
#endif
	return x;
}
