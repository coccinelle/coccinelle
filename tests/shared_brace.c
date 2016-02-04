
int __init ixj_init(void)
{
	if (pci_present()) {
		if ((probe = ixj_probe_pci(&cnt)) < 0) {
		  return probe;
		}
	}
}
