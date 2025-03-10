int __init probe_base_port(int base)
{
	int b = 0x300, e = 0x370;	/* this is the range of start addresses */
	volatile int fool, i;

	if (base)
		b = e = base;
	for (base = b; base <= e; base += 0x10) {
		if (check_region(base, 0x10))
			continue;
		for (i = 0; i < 3; i++)
			fool = inw(base + 2);	/* empty possibly uart_receive_buffer */
		if ((inw(base + 6) & 0xffef) != 0x0001 ||	/* line_status */
		    (inw(base) & 0xad00) != 0)	/* data status */
			continue;
		return (base);
	}
	return 0;
}

