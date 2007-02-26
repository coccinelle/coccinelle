// pad:  has not been tested for a long time => have bug inside if 

void main(int i)
{

	if (unregister_blkdev(MAJOR_NR, "mcdx") != 0) {
		xwarn("cleanup() unregister_blkdev() failed\n");
	}
	blk_cleanup_queue(&mcdx_queue);
#if !MCDX_QUIET
	else
	xinfo("cleanup() succeeded\n");
#endif

}

void main(int i)
{


	if ((aurora_paranoia_check(port, tty->name, "aurora_ioctl"))
		return -ENODEV;

}

void main(int i)
{

  //but maybe because aurora_paranoia_check is a wierd macro ? 

  //TODO
// = File "test_c/bugs/80/ok/linux-2.6.9/drivers/sbus/char/aurora.c", line 2184, characters 2
//    around = 'return', whole content = 		return;
// charpos = 56405
//FOUND SYNC at line 2202
//badcount: 29
//}

static void aurora_set_termios(struct tty_struct * tty, struct termios * old_termios)
{
	struct Aurora_port *port = (struct Aurora_port *) tty->driver_data;
	unsigned long flags;

#ifdef AURORA_DEBUG
	printk("aurora_set_termios: start\n");
#endif
	if ((aurora_paranoia_check(port, tty->name, "aurora_set_termios"))
		return;

#ifdef AURORA_DEBUG
	printk("aurora_ioctl: start\n");
#endif
	if ((aurora_paranoia_check(port, tty->name, "aurora_ioctl"))
		return -ENODEV;
}

            //zarbfixed? 
            // apparemment manque une ) avant le {.  apparemment pas dans un if 0, donc bug linux ?

            //HANDLING: test_c/bugs/83/ok/linux-2.6.8/drivers/sbus/char/aurora.c
            //parse error 
            // = File "test_c/bugs/83/ok/linux-2.6.8/drivers/sbus/char/aurora.c", line 1418, characters 61
            //    around = '{', whole content = 	if ((aurora_paranoia_check(port, tty->name, "aurora_open")) {
            //charpos = 37474

static void acsi_print_error(const unsigned char *errblk, int struct acsi_info_struct *aip);


	floppy_queue = blk_init_queue(do_fd_request, &floppy_lock)
	if (!floppy_queue)
		goto out_queue;
}
        //pad: bug linux je pense





//parse error 
// = File "test_c/bugs/9/ok/linux-2.5.70/drivers/scsi/ncr53c8xx.c", line 2514, characters 59
//    around = ',', whole content = }/*-------------------------< MSG_WDTR >-----------------*/,{
// charpos = 69409
//FOUND SYNC at line 2539
//pad: BIG error


//HANDLING: test_c/bugs/74/errors/linux-2.6.6/drivers/md/raid5.c
//parse error 
// = File "test_c/bugs/74/errors/linux-2.6.6/drivers/md/raid5.c", line 259, characters 36
//    around = ';', whole content = 						    unplug_slaves(conf->mddev);
// charpos = 7037
//FOUND SYNC at line 283

void main(int i)
{

				wait_event_lock_irq(conf->wait_for_stripe,
						    !list_empty(&conf->inactive_list) &&
						    (atomic_read(&conf->active_stripes) < (NR_STRIPES *3/4)
						     || !conf->inactive_blocked),
						    conf->device_lock,
						    unplug_slaves(conf->mddev);


}




#if defined(CONFIG_MTD_PMC551_APERTURE_SIZE)
static int asize=CONFIG_MTD_PMC551_APERTURE_SIZE
#else
static int asize=0;
#endif




struct dec_serial_hook zs_kgdbhook = {
	.init_channel	= kgdbhook_init_channel,
	.init_info	= kgdbhook_init_info,
	.rx_char	= kgdbhook_rx_char,
	.cflags		= B38400 | CS8 | CLOCAL,
}

// Miss ptvirg

void __init zs_kgdb_hook(int tty_num)
{
	/* Find out how many Z8530 SCCs we have */

}
