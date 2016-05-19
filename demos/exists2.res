#include <stdio.h>

int bar(void)
{
	int a;

	a = 300;
	b();
}

int main(void)
{
	int a;

	a = 10;
	b();

	c = 400;
	if (a > 5)
		c();

	return 0;
}
