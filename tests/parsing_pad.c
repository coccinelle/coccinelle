#ifdef SIGALRM
#if defined(__STDC__) || defined(sgi) || defined(_AIX)
#define SIGRETTYPE void*
#else
#define SIGRETTYPE int*
#endif


SIGRETTYPE foo(void) 
{
	void x;

	int x$y;
}
