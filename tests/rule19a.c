static void
gazel_interrupt_ipac(int intno, void *dev_id, struct pt_regs *regs)
{
	struct IsdnCardState *cs = dev_id;
	u_char ista, val;
	int count = 0;

	if (!cs) {
		printk(KERN_WARNING "Gazel: Spurious interrupt!\n");
		return;
	}
	do {
		if (ista & 0x10) {
			val = 0x01;
			isac_interrupt(cs, val);
		}
	}
	while ((ista & 0x3f) && (count < MAXCOUNT));

	WriteISAC(cs, IPAC_MASK - 0x80, 0xFF);
	WriteISAC(cs, IPAC_MASK - 0x80, 0xC0);
}

