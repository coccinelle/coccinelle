void main(int i)
{

#if MEMORY_MAPPED_IO
	ret = readw(unsigned long) port;
#else
	ret = inw((unsigned long) port);
#endif

	ret = readw(unsigned long) port;
#else
	ret = inw((unsigned long) port);


}
