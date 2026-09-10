struct regs {
	int and;
	int or;
	int xor;
};

int masks(struct regs *r, int *p, int and, int or)
{
	int val = *p;

	val &= and;
	val |= or;
	r->xor = val ^ r->and;
	p = kmalloc(sizeof(*p), GFP_KERNEL);
	return val;
}

int negate(int not, int compl, int *p)
{
	p = kmalloc(sizeof(*p), GFP_KERNEL);
	return not + compl;
}

int bits(int bitand, int bitor, int *p)
{
	p = kmalloc(sizeof(*p), GFP_KERNEL);
	return bitand + bitor;
}
