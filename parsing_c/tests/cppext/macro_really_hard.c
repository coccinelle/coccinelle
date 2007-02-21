#ifdef __KERNEL__
#define x(a) st_ ## a
#define y enum
#else
#define x(a) #a
#define y char * stats_name[] = 
#endif

y {x(interrupt), x(data_ready), x(fifo_overflow), x(data_error),
     x(crc_error), x(sync_error), x(lost_intr), x(echo),
     x(write_timeout), x(receive_timeout), x(read_timeout),
     x(dsb_timeout), x(stop_0xff), x(back_read_timeout),
     x(sector_transferred), x(read_restarted), x(read_background),
     x(bh), x(open), x(ioctl_multisession), x(attention)
#ifdef __KERNEL__
     , x(last_entry)
#endif
 };

void main(int i)
{
	IFDEBUG(1) printk(KERN_INFO "Arlan: memory tests ok\n");


	BUGLVL(D_NORMAL) printk(VERSION);


	PRINTP("SCSI host number %d : %s\n" ANDP scsi_ptr->host_no ANDP scsi_ptr->hostt->name);
}
