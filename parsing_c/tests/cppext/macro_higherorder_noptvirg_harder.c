void main(int i)
{

  READ_LOCK_IRQSAVE(&pCh->Ibuf_spinlock,flags)
  if ( pCh->Ibuf_stuff != pCh->Ibuf_strip ) {
  READ_UNLOCK_IRQRESTORE(&pCh->Ibuf_spinlock,flags)
    }
}

#define Debug(a) {printk (KERN_DEBUG); printk a;}
#ifdef DEBUG
#define debug(a) Debug(a)
#else
#define debug(a) (void) 0;
#endif

void main(int i)
{


  if (tries>1) debug(("Took me some tries\n"))
  else if (tries == MAX_TRIES) 
    debug(("MAX_TRIES tries for read sector\n"));

}



#ifdef STATISTICS		/* keep track of errors in counters */
#define stats(i) { ++cd->stats[st_ ## i]; \
		     cd->last_stat[st_ ## i] = cd->stat_counter++; \
		 }
#else
#define stats(i) (void) 0;
#endif

void main(int i)
{

		if (timeout == UART_TIMEOUT)
			stats(receive_timeout)	/* no `;'! */
			    else
			stats(dsb_timeout);
}


void main(int i)
{


		NTFS_GETOPT("uid", uid)
		else NTFS_GETOPT("gid", gid)

}


void main(int i)
{


#ifdef STATISTICS
		if (timeout == UART_TIMEOUT)
			stats(receive_timeout)	/* no `;'! */
			    else
			stats(dsb_timeout);
#endif

}
