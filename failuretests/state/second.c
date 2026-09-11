struct item { int x; };

void second(void)
{
	struct item *p;

	p = kmalloc(sizeof(*p), GFP_KERNEL);
}
