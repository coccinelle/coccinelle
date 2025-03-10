void pcibios_report_status(u_int status_mask, int warn)
{
        struct list_head *l;

        list_for_each(l, &pci_root_buses) {
                struct pci_bus *bus = pci_bus_b(l);

                pcibios_bus_report_status(bus, status_mask, warn);
        }
}
