
static inline void alloc_resource(struct pci_dev *dev, int idx)
{
	struct resource *pr, *r = &dev->resource[idx];

	if (pr)
		DBG("PCI");
	else pr = NULL;
}
