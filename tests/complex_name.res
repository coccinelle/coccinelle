struct data {
	int *complex;
};

void field(struct data *d, int *p)
{
	p = kmalloc_obj(*p, GFP_KERNEL);
	d->complex = p;
}

int *param(int *complex, int *p)
{
	p = kmalloc_obj(*p, GFP_KERNEL);
	return complex;
}

void types(int *p)
{
	double complex a;
	float complex b;
	long double complex c;
	complex double d;

	p = kmalloc_obj(*p, GFP_KERNEL);
}
