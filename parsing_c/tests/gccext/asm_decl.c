static u16 mm_inw(unsigned long a)
{
 	register u16 r __asm__("er0");
	__asm__("mov.w @%1,r2\n\t"
		"mov.b r2l,%x0\n\t"
		"mov.b r2h,%w0"
		:"=r"(r)
		:"r"(a)
		:"er2");
	return r;
}

