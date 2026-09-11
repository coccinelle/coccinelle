struct dev {
	void __iomem *base;
};

void f(struct dev *d, void __iomem *p, const int *cp, int *ip)
{
	d->base = p;
	cp = ip;
}
