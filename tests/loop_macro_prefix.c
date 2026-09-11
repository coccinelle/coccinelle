struct holder {
	int *buf;
};

int with_braces(struct holder *h, struct lock *l)
{
	with_lock_held(l, flags) {
		h->buf = kmalloc(sizeof(*h->buf), GFP_KERNEL);
	}
	return 0;
}

int without_braces(struct holder *h, struct lock *l)
{
	with_lock_held(l, flags)
		consume(h);
	h->buf = kmalloc(sizeof(*h->buf), GFP_KERNEL);
	return 0;
}

int scoped(struct holder *h, struct lock *l)
{
	scoped_lock_held(l) {
		h->buf = kmalloc(sizeof(*h->buf), GFP_KERNEL);
	}
	return 0;
}

int scoped_without_braces(struct holder *h, struct lock *l)
{
	scoped_lock_held(l)
		consume(h);
	h->buf = kmalloc(sizeof(*h->buf), GFP_KERNEL);
	return 0;
}

int statement_argument(struct holder *h, struct lock *l)
{
	scoped_cond_lock(l, return -EINTR) {
		h->buf = kmalloc(sizeof(*h->buf), GFP_KERNEL);
	}
	return 0;
}

int guard_single_statement(struct holder *h, struct lock *l)
{
	scoped_guard(mutex, l)
		h->buf = kmalloc(sizeof(*h->buf), GFP_KERNEL);
	return 0;
}

int guard_nested(struct holder *h, struct lock *l)
{
	scoped_lock_held(l)
		scoped_guard(mutex, l) {
			h->buf = kmalloc(sizeof(*h->buf), GFP_KERNEL);
		}
	return 0;
}

int call(struct holder *h, struct lock *l)
{
	with_console(l, consume);
	h->buf = kmalloc(sizeof(*h->buf), GFP_KERNEL);
	return 0;
}
