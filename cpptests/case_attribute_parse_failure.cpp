int main()
{
	int i {};
	switch ( i )
	{
		[[likely]]
		case 0: 0; break;
		[[unlikely]]
		case 1: 0;
	}
}
