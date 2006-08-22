static void
asuscom_interrupt_ipac(int intno, void *dev_id, struct pt_regs *regs)
{
	if (!cs) {
		printk(KERN_WARNING "ISDNLink: Spurious interrupt!\n");
		return;
	}
Start_IPAC:
	debugl1(cs, "IPAC ISTA %02X", ista);
	if ((ista & 0x3f) && icnt) {
		icnt--;
		goto Start_IPAC;
	}
}
