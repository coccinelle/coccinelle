int __init probe_base_port(int base)
{
	int b = 0x300, e = 0x370;	/* this is the range of start addresses */
	volatile int fool, i;

	if (base)
		b = e = base;
	for (base = b; base <= e; base += 0x10) {
		if (!request_region(base, 0x10, req_reg_arg3))
			continue;
		for (i = 0; i < 3; i++)
			fool = inw(base + 2);	/* empty possibly uart_receive_buffer */
		if ((inw(base + 6) & 0xffef) != 0x0001 ||	/* line_status */
		    (inw(base) & 0xad00) != 0) {
			release_region(base, 0x10);
			continue;
		}
		return (base);
		release_region(base, 0x10);
	}
	return 0;
}

