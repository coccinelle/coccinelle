struct item { int x; };

/* The semantic patch's OCaml script deliberately fails for this function. */
void fail_here(void)
{
	struct item *p;

	p = kmalloc(sizeof(*p), GFP_KERNEL);
}
