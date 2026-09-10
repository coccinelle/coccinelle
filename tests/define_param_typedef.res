#define to_item(type, p) ((type *)(p))

struct holder {
	int *buf;
};

int fill(struct holder *h, int input)
{
	enum kind type;

	if (input)
		type = 1;
	else
		type = 2;
	h->buf = kmalloc_obj(*h->buf, GFP_KERNEL);
	return type;
}
