/* note: this does not compile, but it's parsable by coccinelle */
int f()
{
	return 1;
}

int g()
{
	return 0;
}

int h()
{
	return 1;
}

int l()
{
	co_return 1;
}

int m()
{
	1;
	return;
}

int n()
{
	return;
}

int o()
{
	1;
	return;
}

int p()
{
	1;
	co_return;
}
