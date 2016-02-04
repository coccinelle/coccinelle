static int __init agp_i7x05_probe (struct pci_dev *dev, const struct pci_device_id *ent)
{
	u8 cap_ptr = 0;

	cap_ptr = pci_find_capability(dev, PCI_CAP_ID_AGP);
	if (cap_ptr == 0)
		return -ENODEV;

	if (agp_lookup_host_bridge(dev) != -ENODEV) {
		agp_bridge.dev = dev;
		agp_bridge.capndx = cap_ptr;
		/* Fill in the mode register */
		pci_read_config_dword(agp_bridge.dev, agp_bridge.capndx+PCI_AGP_STATUS, &agp_bridge.mode)
		agp_register_driver(dev);
		return 0;
	}
	return -ENODEV;
}
