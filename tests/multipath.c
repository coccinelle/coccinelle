static void multipathd(struct md_thread *thread)
{
	for (;;) {
		if ((mp_bh->path = multipath_map (conf))<0) {
			printk(KERN_ERR "multipath: %s: redirecting sector %llu"
				" to another IO path\n",
				bdevname(bio->bi_bdev,b),
				(unsigned long long)bio->bi_iter.bi_sector);
		}
	}
}
