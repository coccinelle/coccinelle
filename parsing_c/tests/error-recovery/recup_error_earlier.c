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
