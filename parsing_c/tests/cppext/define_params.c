#define read_roothub(hc, reg, mask) ({ \
	u32 temp = readl (&hc->regs->roothub.reg); \
	if (hc->flags & OHCI_QUIRK_AMD756) \
		while (temp & mask) \
			temp = readl (&hc->regs->roothub.reg); \
	temp; })


//#define read_roothub(hc, register, mask) ({ \
//	u32 temp = readl (&hc->regs->roothub.register); \
//	if (hc->flags & OHCI_QUIRK_AMD756) \
//		while (temp & mask) \
//			temp = readl (&hc->regs->roothub.register); \
//	temp; })


#define MTS_DEBUG_GOT_HERE() \
	MTS_DEBUG("got to %s:%d (%s)\n", __FILE__, (int)__LINE__, __PRETTY_FUNCTION__ )
