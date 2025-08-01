/* note: this does not compile, but it's parsable by coccinelle */
int f()
{
	return 0;
}

int g()
{
	return 0;
}

int h()
{
	co_return 0;
}

int l()
{
	co_return 0;
}

int m()
{
	return;
}

int n()
{
	return;
}

int o()
{
	co_return;
}

int p()
{
	co_return;
}
