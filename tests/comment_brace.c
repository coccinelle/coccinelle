int main ()
{
	while ((inw(base) & 0xad00) != 0)	/* data status */
		continue;
	return 0;
}

