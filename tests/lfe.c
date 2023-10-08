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
	list_for_each_entry(_p, &sr->all_publ, all_publ) {
		if (_p->key == key && (!_p->sk.node || _p->sk.node == node)) {
			pr_debug("Failed to bind duplicate %u,%u,%u/%u:%u/%u\n",
				 p->sr.type, p->sr.lower, p->sr.upper,
				 node, p->sk.ref, key);
			goto exit;
		}
	}
exit:
	return res;
}
