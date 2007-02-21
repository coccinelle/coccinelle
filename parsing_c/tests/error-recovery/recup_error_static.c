'RXD'

// better synchro  stop to next static
static void	SetupRing(SK_AC*, void*, uintptr_t, RXD**, RXD**, RXD**,
			int*, SK_BOOL);

static irqreturn_t SkGeIsr(int irq, void *dev_id, struct pt_regs *ptregs);
static irqreturn_t SkGeIsrOnePort(int irq, void *dev_id, struct pt_regs *ptregs);
static int	SkGeOpen(struct net_device *dev);
