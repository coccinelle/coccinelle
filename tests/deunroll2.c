int good()
{
	for (int i=0;i+4-1<n;i+=4)
	{
		f((i+0)*a[i+0]);
		f((i+1)*a[i+1]);
	}
}

int bad()
{
	for (int i=0;i+4-1<n;i+=4)
	{
		f((i+0)*a[i+0]);
		f((i+1)*a[i+1])+1;
	}
}
