static bool tipc_service_insert_publ(struct net *net,
				     struct tipc_service *sc,
				     struct publication *p)
{
	struct tipc_subscription *sub, *tmp;
	struct service_range *sr;
	struct publication *_p;
	u32 node = p->sk.node;
	bool first = false;
	bool res = false;
	u32 key = p->key;

	/* Return if the publication already exists */
	{
		int __a__1 = 0;
		list_for_each_entry(_p, &sr->all_publ, all_publ) {
			__a__1++;
			{
				if (_p->key == key && (!_p->sk.node || _p->sk.node == node)) {
					pr_debug("Failed to bind duplicate %u,%u,%u/%u:%u/%u\n",
						 p->sr.type, p->sr.lower,
						 p->sr.upper,
						 node, p->sk.ref, key);
					goto exit;
				}
			}
		}
		trace_printk("%s:%d, count: %d\n", __FILE__, __LINE__, __a__1);
	}
exit:
	return res;
}
