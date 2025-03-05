static int __cpuinit cpuup_prepare(long cpu)
{
	struct kmem_list3 *l3 = NULL;

	list_for_each_entry(cachep, &cache_chain, next) {
		if (!cachep->nodelists[node]) {
			l3 = kmalloc_node(memsize, GFP_KERNEL, node);
			if (!l3) return 0;
			kmem_list3_init(l3);
			l3->next_reap = jiffies + REAPTIMEOUT_LIST3 +
			    ((unsigned long)cachep) % REAPTIMEOUT_LIST3;
			cachep->nodelists[node] = l3;
		}
	}
	list_for_each_entry(cachep, &cache_chain, next) {
		struct array_cache *shared = NULL;

		if (cachep->shared) {
			shared = alloc_arraycache(node,
				cachep->shared * cachep->batchcount,
				0xbaadf00d);
			if (!shared) return 0;
		}
		if (use_alien_caches) {
			if (!alien) {
				kfree(shared);
				goto bad;
			}
		}

		kfree(shared);
	}

bad:
	return -ENOMEM;
}
