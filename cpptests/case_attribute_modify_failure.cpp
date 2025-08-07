int main()
{
	int i = 0;
	switch ( i )
	{
		[[likely]]
		case 0: 0; break;
		[[unlikely]]
		case 1: 1; break;
	}
}
