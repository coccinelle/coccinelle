static int smc_probe1(struct net_device *dev, void __iomem *ioaddr,
		      unsigned long irq_flags)
{
        request_irq(irq_flags);
	register_netdev(dev);
}

static int smc_probe2(struct net_device *dev, void __iomem *ioaddr,
		      unsigned long irq_flags)
{
        request_irq(dev);
	register_netdev(dev);
}
