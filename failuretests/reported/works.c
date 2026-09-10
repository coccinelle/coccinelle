struct item { int x; };

void still_works(void)
{
	struct item *p;

	p = kmalloc(sizeof(*p), GFP_KERNEL);
}
