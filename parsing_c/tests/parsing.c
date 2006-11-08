int f(int i) {

#ifdef DEBUG
	printk("hd%c: read: sector %ld, remaining = %ld, buffer=0x%08lx\n",
		dev+'a', CURRENT->sector, CURRENT->nr_sectors,
 		(unsigned long) CURRENT->buffer+512));
#endif

}
