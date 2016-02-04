static irqreturn_t
elmc_interrupt(int irq, void *dev_id, struct pt_regs *reg_ptr)
{
  printk(KERN_ERR "foo",
	 (int) -(((struct pt_regs *) reg_ptr)->orig_eax + 2));
}
