int main ()
{
	while ((inw(base) & 0xad00) != 0)	/* data status */ {
		release_region();
		continue;
	}
	return 0;
}

