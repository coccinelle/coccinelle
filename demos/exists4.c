void bar(void)
{
	int a;

	a = 300;
	b();
	c();
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
