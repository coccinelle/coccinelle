void bar(void)
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

	if (a > 5)
		c();

	return 0;
}
