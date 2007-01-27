/*
 * HP AGPGART routines. 
 */

#include <linux/module.h>
#include <linux/pci.h>
#include <linux/init.h>
#include <linux/agp_backend.h>
#include "agp.h"

#ifndef log2
#define log2(x)		ffz(~(x))
#endif

#define HP_ZX1_IOVA_BASE	GB(1UL)
#define HP_ZX1_IOVA_SIZE	GB(1UL)
#define HP_ZX1_GART_SIZE	(HP_ZX1_IOVA_SIZE / 2)
#define HP_ZX1_SBA_IOMMU_COOKIE	0x0000badbadc0ffeeUL

#define HP_ZX1_PDIR_VALID_BIT	0x8000000000000000UL
#define HP_ZX1_IOVA_TO_PDIR(va)	((va - hp_private.iova_base) >> hp_private.io_tlb_shift)

static struct aper_size_info_fixed hp_zx1_sizes[] =
{
	{0, 0, 0},		/* filled in by hp_zx1_fetch_size() */
};

static struct gatt_mask hp_zx1_masks[] =
{
	{.mask = HP_ZX1_PDIR_VALID_BIT, .type = 0}
};

static struct _hp_private {
	struct pci_dev *ioc;
	volatile u8 *registers;
	u64 *io_pdir;		// PDIR for entire IOVA
	u64 *gatt;		// PDIR just for GART (subset of above)
	u64 gatt_entries;
	u64 iova_base;
	u64 gart_base;
	u64 gart_size;
	u64 io_pdir_size;
	int io_pdir_owner;	// do we own it, or share it with sba_iommu?
	int io_page_size;
	int io_tlb_shift;
	int io_tlb_ps;		// IOC ps config
	int io_pages_per_kpage;
} hp_private;

static int __init hp_zx1_ioc_shared(void)
{
	struct _hp_private *hp = &hp_private;

	printk(KERN_INFO PFX "HP ZX1 IOC: IOPDIR shared with sba_iommu\n");

	/*
	 * IOC already configured by sba_iommu module; just use
	 * its setup.  We assume:
	 * 	- IOVA space is 1Gb in size
	 * 	- first 512Mb is IOMMU, second 512Mb is GART
	 */
	hp->io_tlb_ps = INREG64(hp->registers, HP_ZX1_TCNFG);
	switch (hp->io_tlb_ps) {
		case 0: hp->io_tlb_shift = 12; break;
		case 1: hp->io_tlb_shift = 13; break;
		case 2: hp->io_tlb_shift = 14; break;
		case 3: hp->io_tlb_shift = 16; break;
		default:
			printk(KERN_ERR PFX "Invalid IOTLB page size "
			       "configuration 0x%x\n", hp->io_tlb_ps);
			hp->gatt = 0;
			hp->gatt_entries = 0;
			return -ENODEV;
	}
	hp->io_page_size = 1 << hp->io_tlb_shift;
	hp->io_pages_per_kpage = PAGE_SIZE / hp->io_page_size;

	hp->iova_base = INREG64(hp->registers, HP_ZX1_IBASE) & ~0x1;
	hp->gart_base = hp->iova_base + HP_ZX1_IOVA_SIZE - HP_ZX1_GART_SIZE;

	hp->gart_size = HP_ZX1_GART_SIZE;
	hp->gatt_entries = hp->gart_size / hp->io_page_size;

	hp->io_pdir = phys_to_virt(INREG64(hp->registers, HP_ZX1_PDIR_BASE));
	hp->gatt = &hp->io_pdir[HP_ZX1_IOVA_TO_PDIR(hp->gart_base)];

	if (hp->gatt[0] != HP_ZX1_SBA_IOMMU_COOKIE) {
	    	hp->gatt = 0;
		hp->gatt_entries = 0;
		printk(KERN_ERR PFX "No reserved IO PDIR entry found; "
		       "GART disabled\n");
		return -ENODEV;
	}

	return 0;
}

static int __init hp_zx1_ioc_owner(u8 ioc_rev)
{
	struct _hp_private *hp = &hp_private;

	printk(KERN_INFO PFX "HP ZX1 IOC: IOPDIR dedicated to GART\n");

	/*
	 * Select an IOV page size no larger than system page size.
	 */
	if (PAGE_SIZE >= KB(64)) {
		hp->io_tlb_shift = 16;
		hp->io_tlb_ps = 3;
	} else if (PAGE_SIZE >= KB(16)) {
		hp->io_tlb_shift = 14;
		hp->io_tlb_ps = 2;
	} else if (PAGE_SIZE >= KB(8)) {
		hp->io_tlb_shift = 13;
		hp->io_tlb_ps = 1;
	} else {
		hp->io_tlb_shift = 12;
		hp->io_tlb_ps = 0;
	}
	hp->io_page_size = 1 << hp->io_tlb_shift;
	hp->io_pages_per_kpage = PAGE_SIZE / hp->io_page_size;

	hp->iova_base = HP_ZX1_IOVA_BASE;
	hp->gart_size = HP_ZX1_GART_SIZE;
	hp->gart_base = hp->iova_base + HP_ZX1_IOVA_SIZE - hp->gart_size;

	hp->gatt_entries = hp->gart_size / hp->io_page_size;
	hp->io_pdir_size = (HP_ZX1_IOVA_SIZE / hp->io_page_size) * sizeof(u64);

	return 0;
}

static int __init hp_zx1_ioc_init(void)
{
	struct _hp_private *hp = &hp_private;
	struct pci_dev *ioc;
	int i;
	u8 ioc_rev;

	ioc = pci_find_device(PCI_VENDOR_ID_HP, PCI_DEVICE_ID_HP_ZX1_IOC, NULL);
	if (!ioc) {
		printk(KERN_ERR PFX "Detected HP ZX1 AGP bridge but no IOC\n");
		return -ENODEV;
	}
	hp->ioc = ioc;

	pci_read_config_byte(ioc, PCI_REVISION_ID, &ioc_rev);

	for (i = 0; i < PCI_NUM_RESOURCES; i++) {
		if (pci_resource_flags(ioc, i) == IORESOURCE_MEM) {
			hp->registers = (u8 *) ioremap(pci_resource_start(ioc, i),
						    pci_resource_len(ioc, i));
			break;
		}
	}
	if (!hp->registers) {
		printk(KERN_ERR PFX "Detected HP ZX1 AGP bridge but no CSRs\n");
		return -ENODEV;
	}

	/*
	 * If the IOTLB is currently disabled, we can take it over.
	 * Otherwise, we have to share with sba_iommu.
	 */
	hp->io_pdir_owner = (INREG64(hp->registers, HP_ZX1_IBASE) & 0x1) == 0;

	if (hp->io_pdir_owner)
		return hp_zx1_ioc_owner(ioc_rev);

	return hp_zx1_ioc_shared();
}

static int hp_zx1_fetch_size(void)
{
	int size;

	size = hp_private.gart_size / MB(1);
	hp_zx1_sizes[0].size = size;
	agp_bridge.current_size = (void *) &hp_zx1_sizes[0];
	return size;
}

static int hp_zx1_configure(void)
{
	struct _hp_private *hp = &hp_private;

	agp_bridge.gart_bus_addr = hp->gart_base;
	agp_bridge.capndx = pci_find_capability(agp_bridge.dev, PCI_CAP_ID_AGP);
	pci_read_config_dword(agp_bridge.dev,
		agp_bridge.capndx + PCI_AGP_STATUS, &agp_bridge.mode);

	if (hp->io_pdir_owner) {
		OUTREG64(hp->registers, HP_ZX1_PDIR_BASE,
			virt_to_phys(hp->io_pdir));
		OUTREG64(hp->registers, HP_ZX1_TCNFG, hp->io_tlb_ps);
		OUTREG64(hp->registers, HP_ZX1_IMASK, ~(HP_ZX1_IOVA_SIZE - 1));
		OUTREG64(hp->registers, HP_ZX1_IBASE, hp->iova_base | 0x1);
		OUTREG64(hp->registers, HP_ZX1_PCOM,
			hp->iova_base | log2(HP_ZX1_IOVA_SIZE));
		INREG64(hp->registers, HP_ZX1_PCOM);
	}

	return 0;
}

static void hp_zx1_cleanup(void)
{
	struct _hp_private *hp = &hp_private;

	if (hp->io_pdir_owner)
		OUTREG64(hp->registers, HP_ZX1_IBASE, 0);
	iounmap((void *) hp->registers);
}

static void hp_zx1_tlbflush(agp_memory * mem)
{
	struct _hp_private *hp = &hp_private;

	OUTREG64(hp->registers, HP_ZX1_PCOM, hp->gart_base | log2(hp->gart_size));
	INREG64(hp->registers, HP_ZX1_PCOM);
}

static int hp_zx1_create_gatt_table(void)
{
	struct _hp_private *hp = &hp_private;
	int i;

	if (hp->io_pdir_owner) {
		hp->io_pdir = (u64 *) __get_free_pages(GFP_KERNEL,
						get_order(hp->io_pdir_size));
		if (!hp->io_pdir) {
			printk(KERN_ERR PFX "Couldn't allocate contiguous "
				"memory for I/O PDIR\n");
			hp->gatt = 0;
			hp->gatt_entries = 0;
			return -ENOMEM;
		}
		memset(hp->io_pdir, 0, hp->io_pdir_size);

		hp->gatt = &hp->io_pdir[HP_ZX1_IOVA_TO_PDIR(hp->gart_base)];
	}

	for (i = 0; i < hp->gatt_entries; i++) {
		hp->gatt[i] = (unsigned long) agp_bridge.scratch_page;
	}

	return 0;
}

static int hp_zx1_free_gatt_table(void)
{
	struct _hp_private *hp = &hp_private;

	if (hp->io_pdir_owner)
		free_pages((unsigned long) hp->io_pdir,
			    get_order(hp->io_pdir_size));
	else
		hp->gatt[0] = HP_ZX1_SBA_IOMMU_COOKIE;
	return 0;
}

static int hp_zx1_insert_memory(agp_memory * mem, off_t pg_start, int type)
{
	struct _hp_private *hp = &hp_private;
	int i, k;
	off_t j, io_pg_start;
	int io_pg_count;

	if (type != 0 || mem->type != 0) {
		return -EINVAL;
	}

	io_pg_start = hp->io_pages_per_kpage * pg_start;
	io_pg_count = hp->io_pages_per_kpage * mem->page_count;
	if ((io_pg_start + io_pg_count) > hp->gatt_entries) {
		return -EINVAL;
	}

	j = io_pg_start;
	while (j < (io_pg_start + io_pg_count)) {
		if (hp->gatt[j]) {
			return -EBUSY;
		}
		j++;
	}

	if (mem->is_flushed == FALSE) {
		CACHE_FLUSH();
		mem->is_flushed = TRUE;
	}

	for (i = 0, j = io_pg_start; i < mem->page_count; i++) {
		unsigned long paddr;

		paddr = mem->memory[i];
		for (k = 0;
		     k < hp->io_pages_per_kpage;
		     k++, j++, paddr += hp->io_page_size) {
			hp->gatt[j] = agp_bridge.mask_memory(paddr, type);
		}
	}

	agp_bridge.tlb_flush(mem);
	return 0;
}

static int hp_zx1_remove_memory(agp_memory * mem, off_t pg_start, int type)
{
	struct _hp_private *hp = &hp_private;
	int i, io_pg_start, io_pg_count;

	if (type != 0 || mem->type != 0) {
		return -EINVAL;
	}

	io_pg_start = hp->io_pages_per_kpage * pg_start;
	io_pg_count = hp->io_pages_per_kpage * mem->page_count;
	for (i = io_pg_start; i < io_pg_count + io_pg_start; i++) {
		hp->gatt[i] = agp_bridge.scratch_page;
	}

	agp_bridge.tlb_flush(mem);
	return 0;
}

static unsigned long hp_zx1_mask_memory(unsigned long addr, int type)
{
	return HP_ZX1_PDIR_VALID_BIT | addr;
}

static int __init hp_zx1_setup (struct pci_dev *pdev __attribute__((unused)))
{
	agp_bridge.masks = hp_zx1_masks;
	agp_bridge.dev_private_data = NULL;
	agp_bridge.size_type = FIXED_APER_SIZE;
	agp_bridge.needs_scratch_page = FALSE;
	agp_bridge.configure = hp_zx1_configure;
	agp_bridge.fetch_size = hp_zx1_fetch_size;
	agp_bridge.cleanup = hp_zx1_cleanup;
	agp_bridge.tlb_flush = hp_zx1_tlbflush;
	agp_bridge.mask_memory = hp_zx1_mask_memory;
	agp_bridge.agp_enable = agp_generic_agp_enable;
	agp_bridge.cache_flush = global_cache_flush;
	agp_bridge.create_gatt_table = hp_zx1_create_gatt_table;
	agp_bridge.free_gatt_table = hp_zx1_free_gatt_table;
	agp_bridge.insert_memory = hp_zx1_insert_memory;
	agp_bridge.remove_memory = hp_zx1_remove_memory;
	agp_bridge.alloc_by_type = agp_generic_alloc_by_type;
	agp_bridge.free_by_type = agp_generic_free_by_type;
	agp_bridge.agp_alloc_page = agp_generic_alloc_page;
	agp_bridge.agp_destroy_page = agp_generic_destroy_page;
	agp_bridge.cant_use_aperture = 1;
	return hp_zx1_ioc_init();
}

static int __init agp_find_supported_device(struct pci_dev *dev)
{
	agp_bridge.dev = dev;

	/* ZX1 LBAs can be either PCI or AGP bridges */
	if (pci_find_capability(dev, PCI_CAP_ID_AGP)) {
		printk(KERN_INFO PFX "Detected HP ZX1 AGP chipset at %s\n",
			dev->slot_name);
		agp_bridge.type = HP_ZX1;
		agp_bridge.dev = dev;
		return hp_zx1_setup(dev);
	}
	return -ENODEV;
}

static struct agp_driver hp_agp_driver = {
	.owner = THIS_MODULE
};

static int __init agp_hp_probe (struct pci_dev *dev, const struct pci_device_id *ent)
{
	if (agp_find_supported_device(dev) == 0) {
		hp_agp_driver.dev = dev;
		agp_register_driver(&hp_agp_driver);
		return 0;
	}
	return -ENODEV;
}

static struct pci_device_id agp_hp_pci_table[] __initdata = {
	{
	.class		= (PCI_CLASS_BRIDGE_HOST << 8),
	.class_mask	= ~0,
	.vendor		= PCI_VENDOR_ID_HP,
	.device		= PCI_DEVICE_ID_HP_ZX1_LBA,
	.subvendor	= PCI_ANY_ID,
	.subdevice	= PCI_ANY_ID,
	},
	{ }
};

MODULE_DEVICE_TABLE(pci, agp_pci_table);

static struct __initdata pci_driver agp_hp_pci_driver = {
	.name		= "agpgart-hp",
	.id_table	= agp_hp_pci_table,
	.probe		= agp_hp_probe,
};

static int __init agp_hp_init(void)
{
	int ret_val;

	ret_val = pci_module_init(&agp_hp_pci_driver);
	if (ret_val)
		agp_bridge.type = NOT_SUPPORTED;

	return ret_val;
}

static void __exit agp_hp_cleanup(void)
{
	agp_unregister_driver(&hp_agp_driver);
	pci_unregister_driver(&agp_hp_pci_driver);
}

module_init(agp_hp_init);
module_exit(agp_hp_cleanup);

MODULE_LICENSE("GPL and additional rights");
