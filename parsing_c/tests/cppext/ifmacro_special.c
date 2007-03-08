void main(int i) 
{
 	BUGLVL(D_NORMAL) printk(VERSION);


 	BUGLVL(D_DURING) {
		BUGMSG(D_DURING, "release_arcbuf: freed #%d; buffer queue is now: ",
		       bufnum);
		for (i = lp->next_buf; i != lp->first_free_buf; i = (i+1) % 5)
			BUGMSG2(D_DURING, "#%d ", lp->buf_queue[i]);
		BUGMSG2(D_DURING, "\n");
	}

 	BUGLVL(D_TX) arcnet_dump_packet(dev, lp->next_tx, "go_tx", 0);


 	BUGLVL(D_NORMAL) printk(VERSION);


	BUGLVL(D_DURING)
 	    memset_io(lp->mem_start, 0x42, 2048);
            
        IFDEBUG(ARLAN_DEBUG_TX_CHAIN)
 			printk(KERN_ERR "Retransmit from timer \n");


}
