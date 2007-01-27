#include <linux/module.h>
#include <linux/pci.h>
#include <linux/init.h>
#include <linux/agp_backend.h>
#include "agp.h"

static int intel_7505_fetch_size(void)
{
	int i;
	u16 tmp;
	aper_size_info_16 *values;

	/* 
	 * For AGP 3.0 APSIZE is now 16 bits
	 */
	pci_read_config_word (agp_bridge.dev, INTEL_I7505_APSIZE, &tmp);
	tmp = (tmp & 0xfff);
	
	values = A_SIZE_16(agp_bridge.aperture_sizes);

	for (i=0; i < agp_bridge.num_aperture_sizes; i++) {
		if (tmp == values[i].size_value) {
			agp_bridge.previous_size = agp_bridge.current_size =
					(void *)(values + i);
			agp_bridge.aperture_size_idx = i;
			return values[i].size;
		}
	}
	return 0;
}


static void intel_7505_tlbflush(agp_memory *mem)
{
	u32 temp;
	pci_read_config_dword(agp_bridge.dev, INTEL_I7505_AGPCTRL, &temp);
	pci_write_config_dword(agp_bridge.dev, INTEL_I7505_AGPCTRL, temp & ~(1 << 7));
	pci_read_config_dword(agp_bridge.dev, INTEL_I7505_AGPCTRL, &temp);
	pci_write_config_dword(agp_bridge.dev, INTEL_I7505_AGPCTRL, temp | (1 << 7));
}

static void intel_7505_cleanup(void)
{
	aper_size_info_16 *previous_size;

	previous_size = A_SIZE_16(agp_bridge.previous_size);
	pci_write_config_byte(agp_bridge.dev, INTEL_I7505_APSIZE,
			      previous_size->size_value);
}


static int intel_7505_configure(void)
{
	u32 temp;
	aper_size_info_16 *current_size;
	
	current_size = A_SIZE_16(agp_bridge.current_size);

	/* aperture size */
	pci_write_config_word(agp_bridge.dev, INTEL_I7505_APSIZE,
			      current_size->size_value);

	/* address to map to */
	pci_read_config_dword(agp_bridge.dev, INTEL_I7505_NAPBASELO, &temp);
	agp_bridge.gart_bus_addr = (temp & PCI_BASE_ADDRESS_MEM_MASK);

	/* attbase */ 
	pci_write_config_dword(agp_bridge.dev, INTEL_I7505_ATTBASE,
			       agp_bridge.gatt_bus_addr);

	/* agpctrl */
	pci_write_config_dword(agp_bridge.dev, INTEL_I7505_AGPCTRL, 0x0000);

	/* clear error registers */
	pci_write_config_byte(agp_bridge.dev, INTEL_I7505_ERRSTS, 0xff);
	return 0;
}

static aper_size_info_16 intel_7505_sizes[7] =
{
	{256, 65536, 6, 0xf00},
	{128, 32768, 5, 0xf20},
	{64, 16384, 4, 0xf30},
	{32, 8192, 3, 0xf38},
	{16, 4096, 2, 0xf3c},
	{8, 2048, 1, 0xf3e},
	{4, 1024, 0, 0xf3f}
};

static void i7505_setup (u32 mode)
{
	if ((agp_generic_agp_3_0_enable)==FALSE)
		agp_generic_agp_enable(mode);
}

static int __init intel_7505_setup (struct pci_dev *pdev)
{
	agp_bridge.masks = intel_generic_masks;
	agp_bridge.aperture_sizes = (void *) intel_7505_sizes;
	agp_bridge.size_type = U16_APER_SIZE;
	agp_bridge.num_aperture_sizes = 7;
	agp_bridge.dev_private_data = NULL;
	agp_bridge.needs_scratch_page = FALSE;
	agp_bridge.configure = intel_7505_configure;
	agp_bridge.fetch_size = intel_7505_fetch_size;
	agp_bridge.cleanup = intel_7505_cleanup;
	agp_bridge.tlb_flush = intel_7505_tlbflush;
	agp_bridge.mask_memory = intel_mask_memory;
	agp_bridge.agp_enable = i7505_enable;
	agp_bridge.cache_flush = global_cache_flush;
	agp_bridge.create_gatt_table = agp_generic_create_gatt_table;
	agp_bridge.free_gatt_table = agp_generic_free_gatt_table;
	agp_bridge.insert_memory = agp_generic_insert_memory;
	agp_bridge.remove_memory = agp_generic_remove_memory;
	agp_bridge.alloc_by_type = agp_generic_alloc_by_type;
	agp_bridge.free_by_type = agp_generic_free_by_type;
	agp_bridge.agp_alloc_page = agp_generic_alloc_page;
	agp_bridge.agp_destroy_page = agp_generic_destroy_page;
	agp_bridge.suspend = agp_generic_suspend;
	agp_bridge.resume = agp_generic_resume;
	agp_bridge.cant_use_aperture = 0;
	return 0;
}

struct agp_device_ids i7x05_agp_device_ids[] __initdata =
{
	{
		.device_id	= PCI_DEVICE_ID_INTEL_7505_0,
		.chipset	= INTEL_I7505,
		.chipset_name	= "i7505",
	},
	{
		.device_id	= PCI_DEVICE_ID_INTEL_7205_0,
		.chipset	= INTEL_I7505,
		.chipset_name	= "i7205",
	},
	{ }, /* dummy final entry, always present */
};

/* scan table above for supported devices */
static int __init agp_lookup_host_bridge (struct pci_dev *pdev)
{
	int j=0;
	struct agp_device_ids *devs;
	
	devs = i7x05_agp_device_ids;

	while (devs[j].chipset_name != NULL) {
		if (pdev->device == devs[j].device_id) {
			printk (KERN_INFO PFX "Detected Intel %s chipset\n",
				devs[j].chipset_name);
			agp_bridge.type = devs[j].chipset;

			if (devs[j].chipset_setup != NULL)
				return devs[j].chipset_setup(pdev);
			else
				return intel_7505_setup(pdev);
		}
		j++;
	}

	printk(KERN_ERR PFX "Unsupported Intel chipset (device id: %04x),",
		pdev->device);
	return -ENODEV;
}

static struct agp_driver i7x05_agp_driver = {
	.owner = THIS_MODULE
};

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
		i7x05_agp_driver.dev = dev;
		agp_register_driver(&i7x05_agp_driver);
		return 0;
	}
	return -ENODEV;
}


static struct pci_device_id agp_i7x05_pci_table[] __initdata = {
	{
	.class		= (PCI_CLASS_BRIDGE_HOST << 8),
	.class_mask	= ~0,
	.vendor		= PCI_VENDOR_ID_INTEL,
	.device		= PCI_ANY_ID,
	.subvendor	= PCI_ANY_ID,
	.subdevice	= PCI_ANY_ID,
	},
	{ }
};

MODULE_DEVICE_TABLE(pci, agp_i7x05_pci_table);

static struct __initdata pci_driver agp_i7x05_pci_driver = {
	.name		= "agpgart-i7x05",
	.id_table	= agp_i7x05_pci_table,
	.probe		= agp_i7x05_probe,
};

int __init agp_i7x05_init(void)
{
	int ret_val;

	ret_val = pci_module_init(&agp_i7x05_pci_driver);
	if (ret_val)
		agp_bridge.type = NOT_SUPPORTED;

	return ret_val;
}

static void __exit agp_i7x05_cleanup(void)
{
	agp_unregister_driver(&i7x05_agp_driver);
	pci_unregister_driver(&agp_i7x05_pci_driver);
}

module_init(agp_i7x05_init);
module_exit(agp_i7x05_cleanup);

MODULE_AUTHOR("Matthew E Tolentino <matthew.e.tolentino@intel.com>");
MODULE_LICENSE("GPL and additional rights");

