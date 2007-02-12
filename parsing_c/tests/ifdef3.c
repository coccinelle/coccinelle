static int bluetooth_write (struct tty_struct * tty, int from_user, const unsigned char *buf, int count)
{
	if (count == 1) {
		dbg(__FUNCTION__ " - write request only included type %d", buf[0]);
		return 1;
	}

#ifdef DEBUG
	printk (KERN_DEBUG __FILE__ ": " __FUNCTION__ " - length = %d, data = ", count);
	for (i = 0; i < count; ++i) {
		printk ("%.2x ", buf[i]);
	}
	printk ("\n");
#endif


exit:
	if (temp_buffer != NULL)
		kfree (temp_buffer);

	return retval;
}
