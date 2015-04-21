static irqreturn_t
snd_ad1889_interrupt(void *dev_id, int x,
		     struct pt_regs *regs)
{

	return IRQ_HANDLED;
}

static irqreturn_t
snd_ad1889_interrupt(void *dev_id,
		     struct pt_regs *regs)
{

	return IRQ_HANDLED;
}
