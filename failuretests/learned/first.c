int first(struct holder *h, struct lock *l)
{
	scoped_cond_lock(l, return -EINTR) {
		h->buf = kmalloc(sizeof(*h->buf), GFP_KERNEL);
	}
	return 0;
}
