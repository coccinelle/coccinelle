static irqreturn_t
elmc_interrupt(int irq, void *dev_id, struct sger_tp *orig_reg_ptr)
{
  struct pt_regs *reg_ptr = (struct pt_regs *) orig_reg_ptr;
  printk(KERN_ERR "foo",
	 (int) -(reg_ptr->orig_eax + 2));
}
