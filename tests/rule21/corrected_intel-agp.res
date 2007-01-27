/*
 * Intel AGPGART routines.
 */

#include <linux/module.h>
#include <linux/pci.h>
#include <linux/init.h>
#include <linux/agp_backend.h>
#include "agp.h"

static int agp_try_unsupported __initdata = 0;


static struct aper_size_info_fixed intel_i810_sizes[] =
{
	{64, 16384, 4},
	/* The 32M mode still requires a 64k gatt */
	{32, 8192, 4}
};

#define AGP_DCACHE_MEMORY	1
#define AGP_PHYS_MEMORY		2

static struct gatt_mask intel_i810_masks[] =
{
	{.mask = I810_PTE_VALID, .type = 0},
	{.mask = (I810_PTE_VALID | I810_PTE_LOCAL), .type = AGP_DCACHE_MEMORY},
	{.mask = I810_PTE_VALID, .type = 0}
};

static struct _intel_i810_private {
	struct pci_dev *i810_dev;	/* device one */
	volatile u8 *registers;
	int num_dcache_entries;
} intel_i810_private;

static int intel_i810_fetch_size(void)
{
	u32 smram_miscc;
	struct aper_size_info_fixed *values;

	pci_read_config_dword(agp_bridge.dev, I810_SMRAM_MISCC, &smram_miscc);
	values = A_SIZE_FIX(agp_bridge.aperture_sizes);

	if ((smram_miscc & I810_GMS) == I810_GMS_DISABLE) {
		printk(KERN_WARNING PFX "i810 is disabled\n");
		return 0;
	}
	if ((smram_miscc & I810_GFX_MEM_WIN_SIZE) == I810_GFX_MEM_WIN_32M) {
		agp_bridge.previous_size =
			agp_bridge.current_size = (void *) (values + 1);
		agp_bridge.aperture_size_idx = 1;
		return values[1].size;
	} else {
		agp_bridge.previous_size =
			agp_bridge.current_size = (void *) (values);
		agp_bridge.aperture_size_idx = 0;
		return values[0].size;
	}

	return 0;
}

static int intel_i810_configure(void)
{
	struct aper_size_info_fixed *current_size;
	u32 temp;
	int i;

	current_size = A_SIZE_FIX(agp_bridge.current_size);

	pci_read_config_dword(intel_i810_private.i810_dev, I810_MMADDR, &temp);
	temp &= 0xfff80000;

	intel_i810_private.registers = (volatile u8 *) ioremap(temp, 128 * 4096);

	if ((INREG32(intel_i810_private.registers, I810_DRAM_CTL)
		& I810_DRAM_ROW_0) == I810_DRAM_ROW_0_SDRAM) {
		/* This will need to be dynamically assigned */
		printk(KERN_INFO PFX "detected 4MB dedicated video ram.\n");
		intel_i810_private.num_dcache_entries = 1024;
	}
	pci_read_config_dword(intel_i810_private.i810_dev, I810_GMADDR, &temp);
	agp_bridge.gart_bus_addr = (temp & PCI_BASE_ADDRESS_MEM_MASK);
	OUTREG32(intel_i810_private.registers, I810_PGETBL_CTL,
		 agp_bridge.gatt_bus_addr | I810_PGETBL_ENABLED);
	CACHE_FLUSH();

	if (agp_bridge.needs_scratch_page == TRUE) {
		for (i = 0; i < current_size->num_entries; i++) {
			OUTREG32(intel_i810_private.registers,
				 I810_PTE_BASE + (i * 4),
				 agp_bridge.scratch_page);
		}
	}
	return 0;
}

static void intel_i810_cleanup(void)
{
	OUTREG32(intel_i810_private.registers, I810_PGETBL_CTL, 0);
	iounmap((void *) intel_i810_private.registers);
}

static void intel_i810_tlbflush(agp_memory * mem)
{
	return;
}

static void intel_i810_agp_enable(u32 mode)
{
	return;
}

static int intel_i810_insert_entries(agp_memory * mem, off_t pg_start,
				int type)
{
	int i, j, num_entries;
	void *temp;

	temp = agp_bridge.current_size;
	num_entries = A_SIZE_FIX(temp)->num_entries;

	if ((pg_start + mem->page_count) > num_entries) {
		return -EINVAL;
	}
	for (j = pg_start; j < (pg_start + mem->page_count); j++) {
		if (!PGE_EMPTY(agp_bridge.gatt_table[j])) {
			return -EBUSY;
		}
	}

	if (type != 0 || mem->type != 0) {
		if ((type == AGP_DCACHE_MEMORY) && (mem->type == AGP_DCACHE_MEMORY)) {
			/* special insert */
			CACHE_FLUSH();
			for (i = pg_start; i < (pg_start + mem->page_count); i++) {
				OUTREG32(intel_i810_private.registers,
					 I810_PTE_BASE + (i * 4),
					 (i * 4096) | I810_PTE_LOCAL |
					 I810_PTE_VALID);
			}
			CACHE_FLUSH();
			agp_bridge.tlb_flush(mem);
			return 0;
		}
		if((type == AGP_PHYS_MEMORY) && (mem->type == AGP_PHYS_MEMORY))
			goto insert;
		return -EINVAL;
	}

insert:
	CACHE_FLUSH();
	for (i = 0, j = pg_start; i < mem->page_count; i++, j++) {
		OUTREG32(intel_i810_private.registers,
			I810_PTE_BASE + (j * 4),
			agp_bridge.mask_memory(mem->memory[i], mem->type));
	}
	CACHE_FLUSH();

	agp_bridge.tlb_flush(mem);
	return 0;
}

static int intel_i810_remove_entries(agp_memory * mem, off_t pg_start,
				int type)
{
	int i;

	for (i = pg_start; i < (mem->page_count + pg_start); i++) {
		OUTREG32(intel_i810_private.registers,
			 I810_PTE_BASE + (i * 4),
			 agp_bridge.scratch_page);
	}

	CACHE_FLUSH();
	agp_bridge.tlb_flush(mem);
	return 0;
}

static agp_memory *intel_i810_alloc_by_type(size_t pg_count, int type)
{
	agp_memory *new;

	if (type == AGP_DCACHE_MEMORY) {
		if (pg_count != intel_i810_private.num_dcache_entries) {
			return NULL;
		}
		new = agp_create_memory(1);

		if (new == NULL) {
			return NULL;
		}
		new->type = AGP_DCACHE_MEMORY;
		new->page_count = pg_count;
		new->num_scratch_pages = 0;
		vfree(new->memory);
		MOD_INC_USE_COUNT;
		return new;
	}
	if(type == AGP_PHYS_MEMORY) {
		void *addr;
		/* The I810 requires a physical address to program
		 * it's mouse pointer into hardware.  However the
		 * Xserver still writes to it through the agp
		 * aperture
		 */
		if (pg_count != 1)
			return NULL;

		new = agp_create_memory(1);
		if (new == NULL)
			return NULL;

		MOD_INC_USE_COUNT;
		addr = agp_bridge.agp_alloc_page();

		if (addr == NULL) {
			/* Free this structure */
			agp_free_memory(new);
			return NULL;
		}
		new->memory[0] = virt_to_phys(addr);
		new->page_count = 1;
		new->num_scratch_pages = 1;
		new->type = AGP_PHYS_MEMORY;
		new->physical = new->memory[0];
		return new;
	}
	return NULL;
}

static void intel_i810_free_by_type(agp_memory * curr)
{
	agp_free_key(curr->key);
	if(curr->type == AGP_PHYS_MEMORY) {
		agp_bridge.agp_destroy_page(phys_to_virt(curr->memory[0]));
		vfree(curr->memory);
	}
	kfree(curr);
	MOD_DEC_USE_COUNT;
}

static unsigned long intel_i810_mask_memory(unsigned long addr, int type)
{
	/* Type checking must be done elsewhere */
	return addr | agp_bridge.masks[type].mask;
}

static int __init intel_i810_setup(struct pci_dev *i810_dev)
{
	intel_i810_private.i810_dev = i810_dev;

	agp_bridge.masks = intel_i810_masks;
	agp_bridge.aperture_sizes = (void *) intel_i810_sizes;
	agp_bridge.size_type = FIXED_APER_SIZE;
	agp_bridge.num_aperture_sizes = 2;
	agp_bridge.dev_private_data = (void *) &intel_i810_private;
	agp_bridge.needs_scratch_page = TRUE;
	agp_bridge.configure = intel_i810_configure;
	agp_bridge.fetch_size = intel_i810_fetch_size;
	agp_bridge.cleanup = intel_i810_cleanup;
	agp_bridge.tlb_flush = intel_i810_tlbflush;
	agp_bridge.mask_memory = intel_i810_mask_memory;
	agp_bridge.agp_enable = intel_i810_agp_enable;
	agp_bridge.cache_flush = global_cache_flush;
	agp_bridge.create_gatt_table = agp_generic_create_gatt_table;
	agp_bridge.free_gatt_table = agp_generic_free_gatt_table;
	agp_bridge.insert_memory = intel_i810_insert_entries;
	agp_bridge.remove_memory = intel_i810_remove_entries;
	agp_bridge.alloc_by_type = intel_i810_alloc_by_type;
	agp_bridge.free_by_type = intel_i810_free_by_type;
	agp_bridge.agp_alloc_page = agp_generic_alloc_page;
	agp_bridge.agp_destroy_page = agp_generic_destroy_page;
	agp_bridge.suspend = agp_generic_suspend;
	agp_bridge.resume = agp_generic_resume;
	agp_bridge.cant_use_aperture = 0;

	return 0;
}

static struct aper_size_info_fixed intel_i830_sizes[] =
{
	{128, 32768, 5},
	/* The 64M mode still requires a 128k gatt */
	{64, 16384, 5}
};

static struct _intel_i830_private {
	struct pci_dev *i830_dev;		/* device one */
	volatile u8 *registers;
	int gtt_entries;
} intel_i830_private;

static void intel_i830_init_gtt_entries(void)
{
	u16 gmch_ctrl;
	int gtt_entries;
	u8 rdct;
	static const int ddt[4] = { 0, 16, 32, 64 };

	pci_read_config_word(agp_bridge.dev,I830_GMCH_CTRL,&gmch_ctrl);

	switch (gmch_ctrl & I830_GMCH_GMS_MASK) {
	case I830_GMCH_GMS_STOLEN_512:
		gtt_entries = KB(512) - KB(132);
		printk(KERN_INFO PFX "detected %dK stolen memory.\n",gtt_entries / KB(1));
		break;
	case I830_GMCH_GMS_STOLEN_1024:
		gtt_entries = MB(1) - KB(132);
		printk(KERN_INFO PFX "detected %dK stolen memory.\n",gtt_entries / KB(1));
		break;
	case I830_GMCH_GMS_STOLEN_8192:
		gtt_entries = MB(8) - KB(132);
		printk(KERN_INFO PFX "detected %dK stolen memory.\n",gtt_entries / KB(1));
		break;
	case I830_GMCH_GMS_LOCAL:
		rdct = INREG8(intel_i830_private.registers,I830_RDRAM_CHANNEL_TYPE);
		gtt_entries = (I830_RDRAM_ND(rdct) + 1) * MB(ddt[I830_RDRAM_DDT(rdct)]);
		printk(KERN_INFO PFX "detected %dK local memory.\n",gtt_entries / KB(1));
		break;
	default:
		printk(KERN_INFO PFX "no video memory detected.\n");
		gtt_entries = 0;
		break;
	}

	gtt_entries /= KB(4);

	intel_i830_private.gtt_entries = gtt_entries;
}

/* The intel i830 automatically initializes the agp aperture during POST.
 * Use the memory already set aside for in the GTT.
 */
static int intel_i830_create_gatt_table(void)
{
	int page_order;
	struct aper_size_info_fixed *size;
	int num_entries;
	u32 temp;

	size = agp_bridge.current_size;
	page_order = size->page_order;
	num_entries = size->num_entries;
	agp_bridge.gatt_table_real = 0;

	pci_read_config_dword(intel_i830_private.i830_dev,I810_MMADDR,&temp);
	temp &= 0xfff80000;

	intel_i830_private.registers = (volatile u8 *) ioremap(temp,128 * 4096);
	if (!intel_i830_private.registers) return (-ENOMEM);

	temp = INREG32(intel_i830_private.registers,I810_PGETBL_CTL) & 0xfffff000;
	CACHE_FLUSH();

	/* we have to call this as early as possible after the MMIO base address is known */
	intel_i830_init_gtt_entries();

	agp_bridge.gatt_table = NULL;

	agp_bridge.gatt_bus_addr = temp;

	return(0);
}

/* Return the gatt table to a sane state. Use the top of stolen
 * memory for the GTT.
 */
static int intel_i830_free_gatt_table(void)
{
	return(0);
}

static int intel_i830_fetch_size(void)
{
	u16 gmch_ctrl;
	struct aper_size_info_fixed *values;

	pci_read_config_word(agp_bridge.dev,I830_GMCH_CTRL,&gmch_ctrl);
	values = A_SIZE_FIX(agp_bridge.aperture_sizes);

	if ((gmch_ctrl & I830_GMCH_MEM_MASK) == I830_GMCH_MEM_128M) {
		agp_bridge.previous_size = agp_bridge.current_size = (void *) values;
		agp_bridge.aperture_size_idx = 0;
		return(values[0].size);
	} else {
		agp_bridge.previous_size = agp_bridge.current_size = (void *) values;
		agp_bridge.aperture_size_idx = 1;
		return(values[1].size);
	}

	return(0);
}

static int intel_i830_configure(void)
{
	struct aper_size_info_fixed *current_size;
	u32 temp;
	u16 gmch_ctrl;
	int i;

	current_size = A_SIZE_FIX(agp_bridge.current_size);

	pci_read_config_dword(intel_i830_private.i830_dev,I810_GMADDR,&temp);
	agp_bridge.gart_bus_addr = (temp & PCI_BASE_ADDRESS_MEM_MASK);

	pci_read_config_word(agp_bridge.dev,I830_GMCH_CTRL,&gmch_ctrl);
	gmch_ctrl |= I830_GMCH_ENABLED;
	pci_write_config_word(agp_bridge.dev,I830_GMCH_CTRL,gmch_ctrl);

	OUTREG32(intel_i830_private.registers,I810_PGETBL_CTL,agp_bridge.gatt_bus_addr | I810_PGETBL_ENABLED);
	CACHE_FLUSH();

	if (agp_bridge.needs_scratch_page == TRUE)
		for (i = intel_i830_private.gtt_entries; i < current_size->num_entries; i++)
			OUTREG32(intel_i830_private.registers,I810_PTE_BASE + (i * 4),agp_bridge.scratch_page);

	return (0);
}

static void intel_i830_cleanup(void)
{
	iounmap((void *) intel_i830_private.registers);
}

static int intel_i830_insert_entries(agp_memory *mem,off_t pg_start,int type)
{
	int i,j,num_entries;
	void *temp;

	temp = agp_bridge.current_size;
	num_entries = A_SIZE_FIX(temp)->num_entries;

	if (pg_start < intel_i830_private.gtt_entries) {
		printk (KERN_DEBUG "pg_start == 0x%.8lx,intel_i830_private.gtt_entries == 0x%.8x\n",
				pg_start,intel_i830_private.gtt_entries);

		printk ("Trying to insert into local/stolen memory\n");
		return (-EINVAL);
	}

	if ((pg_start + mem->page_count) > num_entries)
		return (-EINVAL);

	/* The i830 can't check the GTT for entries since its read only,
	 * depend on the caller to make the correct offset decisions.
	 */

	if ((type != 0 && type != AGP_PHYS_MEMORY) ||
		(mem->type != 0 && mem->type != AGP_PHYS_MEMORY))
		return (-EINVAL);

	CACHE_FLUSH();

	for (i = 0, j = pg_start; i < mem->page_count; i++, j++)
		OUTREG32(intel_i830_private.registers,I810_PTE_BASE + (j * 4),
			agp_bridge.mask_memory(mem->memory[i], mem->type));

	CACHE_FLUSH();

	agp_bridge.tlb_flush(mem);

	return(0);
}

static int intel_i830_remove_entries(agp_memory *mem,off_t pg_start,int type)
{
	int i;

	CACHE_FLUSH ();

	if (pg_start < intel_i830_private.gtt_entries) {
		printk ("Trying to disable local/stolen memory\n");
		return (-EINVAL);
	}

	for (i = pg_start; i < (mem->page_count + pg_start); i++)
		OUTREG32(intel_i830_private.registers,I810_PTE_BASE + (i * 4),agp_bridge.scratch_page);

	CACHE_FLUSH();

	agp_bridge.tlb_flush(mem);

	return (0);
}

static agp_memory *intel_i830_alloc_by_type(size_t pg_count,int type)
{
	agp_memory *nw;

	/* always return NULL for now */
	if (type == AGP_DCACHE_MEMORY) return(NULL);

	if (type == AGP_PHYS_MEMORY) {
		void *addr;

		/* The i830 requires a physical address to program
		 * it's mouse pointer into hardware. However the
		 * Xserver still writes to it through the agp
		 * aperture
		 */

		if (pg_count != 1) return(NULL);

		nw = agp_create_memory(1);

		if (nw == NULL) return(NULL);

		MOD_INC_USE_COUNT;
		addr = agp_bridge.agp_alloc_page();
		if (addr == NULL) {
			/* free this structure */
			agp_free_memory(nw);
			return(NULL);
		}

		nw->memory[0] = virt_to_phys(addr);
		nw->page_count = 1;
		nw->num_scratch_pages = 1;
		nw->type = AGP_PHYS_MEMORY;
		nw->physical = nw->memory[0];
		return(nw);
	}

	return(NULL);
}

static int __init intel_i830_setup(struct pci_dev *i830_dev)
{
	intel_i830_private.i830_dev = i830_dev;

	agp_bridge.masks = intel_i810_masks;
	agp_bridge.aperture_sizes = (void *) intel_i830_sizes;
	agp_bridge.size_type = FIXED_APER_SIZE;
	agp_bridge.num_aperture_sizes = 2;

	agp_bridge.dev_private_data = (void *) &intel_i830_private;
	agp_bridge.needs_scratch_page = TRUE;

	agp_bridge.configure = intel_i830_configure;
	agp_bridge.fetch_size = intel_i830_fetch_size;
	agp_bridge.cleanup = intel_i830_cleanup;
	agp_bridge.tlb_flush = intel_i810_tlbflush;
	agp_bridge.mask_memory = intel_i810_mask_memory;
	agp_bridge.agp_enable = intel_i810_agp_enable;
	agp_bridge.cache_flush = global_cache_flush;

	agp_bridge.create_gatt_table = intel_i830_create_gatt_table;
	agp_bridge.free_gatt_table = intel_i830_free_gatt_table;

	agp_bridge.insert_memory = intel_i830_insert_entries;
	agp_bridge.remove_memory = intel_i830_remove_entries;
	agp_bridge.alloc_by_type = intel_i830_alloc_by_type;
	agp_bridge.free_by_type = intel_i810_free_by_type;
	agp_bridge.agp_alloc_page = agp_generic_alloc_page;
	agp_bridge.agp_destroy_page = agp_generic_destroy_page;

	agp_bridge.suspend = agp_generic_suspend;
	agp_bridge.resume = agp_generic_resume;
	agp_bridge.cant_use_aperture = 0;

	return(0);
}
static int intel_fetch_size(void)
{
	int i;
	u16 temp;
	struct aper_size_info_16 *values;

	pci_read_config_word(agp_bridge.dev, INTEL_APSIZE, &temp);
	values = A_SIZE_16(agp_bridge.aperture_sizes);

	for (i = 0; i < agp_bridge.num_aperture_sizes; i++) {
		if (temp == values[i].size_value) {
			agp_bridge.previous_size = agp_bridge.current_size = (void *) (values + i);
			agp_bridge.aperture_size_idx = i;
			return values[i].size;
		}
	}

	return 0;
}

static int intel_8xx_fetch_size(void)
{
	int i;
	u8 temp;
	struct aper_size_info_8 *values;

	pci_read_config_byte(agp_bridge.dev, INTEL_APSIZE, &temp);

	/* Intel 815 chipsets have a _weird_ APSIZE register with only
	 * one non-reserved bit, so mask the others out ... */
	if (agp_bridge.type == INTEL_I815)
		temp &= (1 << 3);

	values = A_SIZE_8(agp_bridge.aperture_sizes);

	for (i = 0; i < agp_bridge.num_aperture_sizes; i++) {
		if (temp == values[i].size_value) {
			agp_bridge.previous_size =
				agp_bridge.current_size = (void *) (values + i);
			agp_bridge.aperture_size_idx = i;
			return values[i].size;
		}
	}
	return 0;
}


static void intel_tlbflush(agp_memory * mem)
{
	pci_write_config_dword(agp_bridge.dev, INTEL_AGPCTRL, 0x2200);
	pci_write_config_dword(agp_bridge.dev, INTEL_AGPCTRL, 0x2280);
}


static void intel_8xx_tlbflush(agp_memory * mem)
{
	u32 temp;
	pci_read_config_dword(agp_bridge.dev, INTEL_AGPCTRL, &temp);
	pci_write_config_dword(agp_bridge.dev, INTEL_AGPCTRL, temp & ~(1 << 7));
	pci_read_config_dword(agp_bridge.dev, INTEL_AGPCTRL, &temp);
	pci_write_config_dword(agp_bridge.dev, INTEL_AGPCTRL, temp | (1 << 7));
}


static void intel_cleanup(void)
{
	u16 temp;
	struct aper_size_info_16 *previous_size;

	previous_size = A_SIZE_16(agp_bridge.previous_size);
	pci_read_config_word(agp_bridge.dev, INTEL_NBXCFG, &temp);
	pci_write_config_word(agp_bridge.dev, INTEL_NBXCFG, temp & ~(1 << 9));
	pci_write_config_word(agp_bridge.dev, INTEL_APSIZE, previous_size->size_value);
}


static void intel_8xx_cleanup(void)
{
	u16 temp;
	struct aper_size_info_8 *previous_size;

	previous_size = A_SIZE_8(agp_bridge.previous_size);
	pci_read_config_word(agp_bridge.dev, INTEL_NBXCFG, &temp);
	pci_write_config_word(agp_bridge.dev, INTEL_NBXCFG, temp & ~(1 << 9));
	pci_write_config_byte(agp_bridge.dev, INTEL_APSIZE, previous_size->size_value);
}


static int intel_configure(void)
{
	u32 temp;
	u16 temp2;
	struct aper_size_info_16 *current_size;

	current_size = A_SIZE_16(agp_bridge.current_size);

	/* aperture size */
	pci_write_config_word(agp_bridge.dev, INTEL_APSIZE, current_size->size_value);

	/* address to map to */
	pci_read_config_dword(agp_bridge.dev, INTEL_APBASE, &temp);
	agp_bridge.gart_bus_addr = (temp & PCI_BASE_ADDRESS_MEM_MASK);

	/* attbase - aperture base */
	pci_write_config_dword(agp_bridge.dev, INTEL_ATTBASE, agp_bridge.gatt_bus_addr);

	/* agpctrl */
	pci_write_config_dword(agp_bridge.dev, INTEL_AGPCTRL, 0x2280);

	/* paccfg/nbxcfg */
	pci_read_config_word(agp_bridge.dev, INTEL_NBXCFG, &temp2);
	pci_write_config_word(agp_bridge.dev, INTEL_NBXCFG,
			(temp2 & ~(1 << 10)) | (1 << 9));
	/* clear any possible error conditions */
	pci_write_config_byte(agp_bridge.dev, INTEL_ERRSTS + 1, 7);
	return 0;
}

static int intel_815_configure(void)
{
	u32 temp, addr;
	u8 temp2;
	struct aper_size_info_8 *current_size;

	current_size = A_SIZE_8(agp_bridge.current_size);

	/* aperture size */
	pci_write_config_byte(agp_bridge.dev, INTEL_APSIZE,
			current_size->size_value); 

	/* address to map to */
	pci_read_config_dword(agp_bridge.dev, INTEL_APBASE, &temp);
	agp_bridge.gart_bus_addr = (temp & PCI_BASE_ADDRESS_MEM_MASK);

	/* attbase - aperture base */
	/* the Intel 815 chipset spec. says that bits 29-31 in the
	* ATTBASE register are reserved -> try not to write them */
	if (agp_bridge.gatt_bus_addr & INTEL_815_ATTBASE_MASK)
		panic("gatt bus addr too high");
	pci_read_config_dword(agp_bridge.dev, INTEL_ATTBASE, &addr);
	addr &= INTEL_815_ATTBASE_MASK;
	addr |= agp_bridge.gatt_bus_addr;
	pci_write_config_dword(agp_bridge.dev, INTEL_ATTBASE, addr);

	/* agpctrl */
	pci_write_config_dword(agp_bridge.dev, INTEL_AGPCTRL, 0x0000); 

	/* apcont */
	pci_read_config_byte(agp_bridge.dev, INTEL_815_APCONT, &temp2);
	pci_write_config_byte(agp_bridge.dev, INTEL_815_APCONT, temp2 | (1 << 1));

	/* clear any possible error conditions */
	/* Oddness : this chipset seems to have no ERRSTS register ! */
	return 0;
}

static void intel_820_tlbflush(agp_memory * mem)
{
	return;
}

static void intel_820_cleanup(void)
{
	u8 temp;
	struct aper_size_info_8 *previous_size;

	previous_size = A_SIZE_8(agp_bridge.previous_size);
	pci_read_config_byte(agp_bridge.dev, INTEL_I820_RDCR, &temp);
	pci_write_config_byte(agp_bridge.dev, INTEL_I820_RDCR, 
			temp & ~(1 << 1));
	pci_write_config_byte(agp_bridge.dev, INTEL_APSIZE,
			previous_size->size_value);
}


static int intel_820_configure(void)
{
	u32 temp;
 	u8 temp2; 
	struct aper_size_info_8 *current_size;

	current_size = A_SIZE_8(agp_bridge.current_size);

	/* aperture size */
	pci_write_config_byte(agp_bridge.dev, INTEL_APSIZE, current_size->size_value); 

	/* address to map to */
	pci_read_config_dword(agp_bridge.dev, INTEL_APBASE, &temp);
	agp_bridge.gart_bus_addr = (temp & PCI_BASE_ADDRESS_MEM_MASK);

	/* attbase - aperture base */
	pci_write_config_dword(agp_bridge.dev, INTEL_ATTBASE, agp_bridge.gatt_bus_addr); 

	/* agpctrl */
	pci_write_config_dword(agp_bridge.dev, INTEL_AGPCTRL, 0x0000); 

	/* global enable aperture access */
	/* This flag is not accessed through MCHCFG register as in */
	/* i850 chipset. */
	pci_read_config_byte(agp_bridge.dev, INTEL_I820_RDCR, &temp2);
	pci_write_config_byte(agp_bridge.dev, INTEL_I820_RDCR, temp2 | (1 << 1));
	/* clear any possible AGP-related error conditions */
	pci_write_config_word(agp_bridge.dev, INTEL_I820_ERRSTS, 0x001c); 
	return 0;
}

static int intel_840_configure(void)
{
	u32 temp;
	u16 temp2;
	struct aper_size_info_8 *current_size;

	current_size = A_SIZE_8(agp_bridge.current_size);

	/* aperture size */
	pci_write_config_byte(agp_bridge.dev, INTEL_APSIZE, current_size->size_value); 

	/* address to map to */
	pci_read_config_dword(agp_bridge.dev, INTEL_APBASE, &temp);
	agp_bridge.gart_bus_addr = (temp & PCI_BASE_ADDRESS_MEM_MASK);

	/* attbase - aperture base */
	pci_write_config_dword(agp_bridge.dev, INTEL_ATTBASE, agp_bridge.gatt_bus_addr); 

	/* agpctrl */
	pci_write_config_dword(agp_bridge.dev, INTEL_AGPCTRL, 0x0000); 

	/* mcgcfg */
	pci_read_config_word(agp_bridge.dev, INTEL_I840_MCHCFG, &temp2);
	pci_write_config_word(agp_bridge.dev, INTEL_I840_MCHCFG, temp2 | (1 << 9));
	/* clear any possible error conditions */
	pci_write_config_word(agp_bridge.dev, INTEL_I840_ERRSTS, 0xc000); 
	return 0;
}

static int intel_845_configure(void)
{
	u32 temp;
	u8 temp2;
	struct aper_size_info_8 *current_size;

	current_size = A_SIZE_8(agp_bridge.current_size);

	/* aperture size */
	pci_write_config_byte(agp_bridge.dev, INTEL_APSIZE, current_size->size_value); 

	/* address to map to */
	pci_read_config_dword(agp_bridge.dev, INTEL_APBASE, &temp);
	agp_bridge.gart_bus_addr = (temp & PCI_BASE_ADDRESS_MEM_MASK);

	/* attbase - aperture base */
	pci_write_config_dword(agp_bridge.dev, INTEL_ATTBASE, agp_bridge.gatt_bus_addr); 

	/* agpctrl */
	pci_write_config_dword(agp_bridge.dev, INTEL_AGPCTRL, 0x0000); 

	/* agpm */
	pci_read_config_byte(agp_bridge.dev, INTEL_I845_AGPM, &temp2);
	pci_write_config_byte(agp_bridge.dev, INTEL_I845_AGPM, temp2 | (1 << 1));
	/* clear any possible error conditions */
	pci_write_config_word(agp_bridge.dev, INTEL_I845_ERRSTS, 0x001c); 
	return 0;
}

static void intel_845_resume(void)
{
	intel_845_configure();
}

static int intel_850_configure(void)
{
	u32 temp;
	u16 temp2;
	struct aper_size_info_8 *current_size;

	current_size = A_SIZE_8(agp_bridge.current_size);

	/* aperture size */
	pci_write_config_byte(agp_bridge.dev, INTEL_APSIZE, current_size->size_value); 

	/* address to map to */
	pci_read_config_dword(agp_bridge.dev, INTEL_APBASE, &temp);
	agp_bridge.gart_bus_addr = (temp & PCI_BASE_ADDRESS_MEM_MASK);

	/* attbase - aperture base */
	pci_write_config_dword(agp_bridge.dev, INTEL_ATTBASE, agp_bridge.gatt_bus_addr); 

	/* agpctrl */
	pci_write_config_dword(agp_bridge.dev, INTEL_AGPCTRL, 0x0000); 

	/* mcgcfg */
	pci_read_config_word(agp_bridge.dev, INTEL_I850_MCHCFG, &temp2);
	pci_write_config_word(agp_bridge.dev, INTEL_I850_MCHCFG, temp2 | (1 << 9));
	/* clear any possible AGP-related error conditions */
	pci_write_config_word(agp_bridge.dev, INTEL_I850_ERRSTS, 0x001c); 
	return 0;
}

static int intel_860_configure(void)
{
	u32 temp;
	u16 temp2;
	struct aper_size_info_8 *current_size;

	current_size = A_SIZE_8(agp_bridge.current_size);

	/* aperture size */
	pci_write_config_byte(agp_bridge.dev, INTEL_APSIZE, current_size->size_value);

	/* address to map to */
	pci_read_config_dword(agp_bridge.dev, INTEL_APBASE, &temp);
	agp_bridge.gart_bus_addr = (temp & PCI_BASE_ADDRESS_MEM_MASK);

	/* attbase - aperture base */
	pci_write_config_dword(agp_bridge.dev, INTEL_ATTBASE, agp_bridge.gatt_bus_addr);

	/* agpctrl */
	pci_write_config_dword(agp_bridge.dev, INTEL_AGPCTRL, 0x0000);

	/* mcgcfg */
	pci_read_config_word(agp_bridge.dev, INTEL_I860_MCHCFG, &temp2);
	pci_write_config_word(agp_bridge.dev, INTEL_I860_MCHCFG, temp2 | (1 << 9));
	/* clear any possible AGP-related error conditions */
	pci_write_config_word(agp_bridge.dev, INTEL_I860_ERRSTS, 0xf700);
	return 0;
}

static int intel_830mp_configure(void)
{
	u32 temp;
	u16 temp2;
	struct aper_size_info_8 *current_size;

	current_size = A_SIZE_8(agp_bridge.current_size);

	/* aperture size */
	pci_write_config_byte(agp_bridge.dev, INTEL_APSIZE, current_size->size_value);

	/* address to map to */
	pci_read_config_dword(agp_bridge.dev, INTEL_APBASE, &temp);
	agp_bridge.gart_bus_addr = (temp & PCI_BASE_ADDRESS_MEM_MASK);

	/* attbase - aperture base */
	pci_write_config_dword(agp_bridge.dev, INTEL_ATTBASE, agp_bridge.gatt_bus_addr);

	/* agpctrl */
	pci_write_config_dword(agp_bridge.dev, INTEL_AGPCTRL, 0x0000);

	/* gmch */
	pci_read_config_word(agp_bridge.dev, INTEL_NBXCFG, &temp2);
	pci_write_config_word(agp_bridge.dev, INTEL_NBXCFG, temp2 | (1 << 9));
	/* clear any possible AGP-related error conditions */
	pci_write_config_word(agp_bridge.dev, INTEL_I830_ERRSTS, 0x1c);
	return 0;
}

static unsigned long intel_mask_memory(unsigned long addr, int type)
{
	/* Memory type is ignored */
	return addr | agp_bridge.masks[0].mask;
}

static void intel_resume(void)
{
	intel_configure();
}

/* Setup function */
static struct gatt_mask intel_generic_masks[] =
{
	{.mask = 0x00000017, .type = 0}
};

static struct aper_size_info_8 intel_815_sizes[2] =
{
	{64, 16384, 4, 0},
	{32, 8192, 3, 8},
};
	
static struct aper_size_info_8 intel_8xx_sizes[7] =
{
	{256, 65536, 6, 0},
	{128, 32768, 5, 32},
	{64, 16384, 4, 48},
	{32, 8192, 3, 56},
	{16, 4096, 2, 60},
	{8, 2048, 1, 62},
	{4, 1024, 0, 63}
};

static struct aper_size_info_16 intel_generic_sizes[7] =
{
	{256, 65536, 6, 0},
	{128, 32768, 5, 32},
	{64, 16384, 4, 48},
	{32, 8192, 3, 56},
	{16, 4096, 2, 60},
	{8, 2048, 1, 62},
	{4, 1024, 0, 63}
};

static struct aper_size_info_8 intel_830mp_sizes[4] = 
{
	{256, 65536, 6, 0},
	{128, 32768, 5, 32},
	{64, 16384, 4, 48},
	{32, 8192, 3, 56}
};

static int __init intel_generic_setup (struct pci_dev *pdev)
{
	agp_bridge.masks = intel_generic_masks;
	agp_bridge.aperture_sizes = (void *) intel_generic_sizes;
	agp_bridge.size_type = U16_APER_SIZE;
	agp_bridge.num_aperture_sizes = 7;
	agp_bridge.dev_private_data = NULL;
	agp_bridge.needs_scratch_page = FALSE;
	agp_bridge.configure = intel_configure;
	agp_bridge.fetch_size = intel_fetch_size;
	agp_bridge.cleanup = intel_cleanup;
	agp_bridge.tlb_flush = intel_tlbflush;
	agp_bridge.mask_memory = intel_mask_memory;
	agp_bridge.agp_enable = agp_generic_agp_enable;
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
	agp_bridge.resume = intel_resume;
	agp_bridge.cant_use_aperture = 0;
	return 0;
}

static int __init intel_815_setup (struct pci_dev *pdev)
{
	agp_bridge.masks = intel_generic_masks;
	agp_bridge.aperture_sizes = (void *) intel_815_sizes;
	agp_bridge.size_type = U8_APER_SIZE;
	agp_bridge.num_aperture_sizes = 2;
	agp_bridge.dev_private_data = NULL;
	agp_bridge.needs_scratch_page = FALSE;
	agp_bridge.configure = intel_815_configure;
	agp_bridge.fetch_size = intel_8xx_fetch_size;
	agp_bridge.cleanup = intel_8xx_cleanup;
	agp_bridge.tlb_flush = intel_8xx_tlbflush;
	agp_bridge.mask_memory = intel_mask_memory;
	agp_bridge.agp_enable = agp_generic_agp_enable;
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


static int __init intel_820_setup (struct pci_dev *pdev)
{
	agp_bridge.masks = intel_generic_masks;
	agp_bridge.aperture_sizes = (void *) intel_8xx_sizes;
	agp_bridge.size_type = U8_APER_SIZE;
	agp_bridge.num_aperture_sizes = 7;
	agp_bridge.dev_private_data = NULL;
	agp_bridge.needs_scratch_page = FALSE;
	agp_bridge.configure = intel_820_configure;
	agp_bridge.fetch_size = intel_8xx_fetch_size;
	agp_bridge.cleanup = intel_820_cleanup;
	agp_bridge.tlb_flush = intel_820_tlbflush;
	agp_bridge.mask_memory = intel_mask_memory;
	agp_bridge.agp_enable = agp_generic_agp_enable;
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

static int __init intel_830mp_setup (struct pci_dev *pdev)
{
	agp_bridge.masks = intel_generic_masks;
	agp_bridge.aperture_sizes = (void *) intel_830mp_sizes;
	agp_bridge.size_type = U8_APER_SIZE;
	agp_bridge.num_aperture_sizes = 4;
	agp_bridge.dev_private_data = NULL;
	agp_bridge.needs_scratch_page = FALSE;
	agp_bridge.configure = intel_830mp_configure;
	agp_bridge.fetch_size = intel_8xx_fetch_size;
	agp_bridge.cleanup = intel_8xx_cleanup;
	agp_bridge.tlb_flush = intel_8xx_tlbflush;
	agp_bridge.mask_memory = intel_mask_memory;
	agp_bridge.agp_enable = agp_generic_agp_enable;
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

static int __init intel_840_setup (struct pci_dev *pdev)
{
	agp_bridge.masks = intel_generic_masks;
	agp_bridge.aperture_sizes = (void *) intel_8xx_sizes;
	agp_bridge.size_type = U8_APER_SIZE;
	agp_bridge.num_aperture_sizes = 7;
	agp_bridge.dev_private_data = NULL;
	agp_bridge.needs_scratch_page = FALSE;
	agp_bridge.configure = intel_840_configure;
	agp_bridge.fetch_size = intel_8xx_fetch_size;
	agp_bridge.cleanup = intel_8xx_cleanup;
	agp_bridge.tlb_flush = intel_8xx_tlbflush;
	agp_bridge.mask_memory = intel_mask_memory;
	agp_bridge.agp_enable = agp_generic_agp_enable;
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

static int __init intel_845_setup (struct pci_dev *pdev)
{
	agp_bridge.masks = intel_generic_masks;
	agp_bridge.aperture_sizes = (void *) intel_8xx_sizes;
	agp_bridge.size_type = U8_APER_SIZE;
	agp_bridge.num_aperture_sizes = 7;
	agp_bridge.dev_private_data = NULL;
	agp_bridge.needs_scratch_page = FALSE;
	agp_bridge.configure = intel_845_configure;
	agp_bridge.fetch_size = intel_8xx_fetch_size;
	agp_bridge.cleanup = intel_8xx_cleanup;
	agp_bridge.tlb_flush = intel_8xx_tlbflush;
	agp_bridge.mask_memory = intel_mask_memory;
	agp_bridge.agp_enable = agp_generic_agp_enable;
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
	agp_bridge.resume = intel_845_resume;
	agp_bridge.cant_use_aperture = 0;
	return 0;
}

static int __init intel_850_setup (struct pci_dev *pdev)
{
	agp_bridge.masks = intel_generic_masks;
	agp_bridge.aperture_sizes = (void *) intel_8xx_sizes;
	agp_bridge.size_type = U8_APER_SIZE;
	agp_bridge.num_aperture_sizes = 7;
	agp_bridge.dev_private_data = NULL;
	agp_bridge.needs_scratch_page = FALSE;
	agp_bridge.configure = intel_850_configure;
	agp_bridge.fetch_size = intel_8xx_fetch_size;
	agp_bridge.cleanup = intel_8xx_cleanup;
	agp_bridge.tlb_flush = intel_8xx_tlbflush;
	agp_bridge.mask_memory = intel_mask_memory;
	agp_bridge.agp_enable = agp_generic_agp_enable;
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

static int __init intel_860_setup (struct pci_dev *pdev)
{
	agp_bridge.masks = intel_generic_masks;
	agp_bridge.aperture_sizes = (void *) intel_8xx_sizes;
	agp_bridge.size_type = U8_APER_SIZE;
	agp_bridge.num_aperture_sizes = 7;
	agp_bridge.dev_private_data = NULL;
	agp_bridge.needs_scratch_page = FALSE;
	agp_bridge.configure = intel_860_configure;
	agp_bridge.fetch_size = intel_8xx_fetch_size;
	agp_bridge.cleanup = intel_8xx_cleanup;
	agp_bridge.tlb_flush = intel_8xx_tlbflush;
	agp_bridge.mask_memory = intel_mask_memory;
	agp_bridge.agp_enable = agp_generic_agp_enable;
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

struct agp_device_ids intel_agp_device_ids[] __initdata =
{
	{
		.device_id	= PCI_DEVICE_ID_INTEL_82443LX_0,
		.chipset	= INTEL_LX,
		.chipset_name	= "440LX",
	},
	{
		.device_id	= PCI_DEVICE_ID_INTEL_82443BX_0,
		.chipset	= INTEL_BX,
		.chipset_name	= "440BX",
	},
	{
		.device_id	= PCI_DEVICE_ID_INTEL_82443GX_0,
		.chipset	= INTEL_GX,
		.chipset_name	= "440GX",
	},
	{
		.device_id	= PCI_DEVICE_ID_INTEL_82815_MC,
		.chipset	= INTEL_I815,
		.chipset_name	= "i815",
		.chipset_setup	= intel_815_setup
	},
	{
		.device_id	= PCI_DEVICE_ID_INTEL_82820_HB,
		.chipset	= INTEL_I820,
		.chipset_name	= "i820",
		.chipset_setup	= intel_820_setup
	},
	{
		.device_id	= PCI_DEVICE_ID_INTEL_82820_UP_HB,
		.chipset	= INTEL_I820,
		.chipset_name	= "i820",
		.chipset_setup	= intel_820_setup
	},
	{
		.device_id	= PCI_DEVICE_ID_INTEL_82830_HB,
		.chipset	= INTEL_I830_M,
		.chipset_name	= "i830M",
		.chipset_setup	= intel_830mp_setup
	},
	{
		.device_id	= PCI_DEVICE_ID_INTEL_82840_HB,
		.chipset	= INTEL_I840,
		.chipset_name	= "i840",
		.chipset_setup	= intel_840_setup
	},
	{
		.device_id	= PCI_DEVICE_ID_INTEL_82845_HB,
		.chipset	= INTEL_I845,
		.chipset_name	= "i845",
		.chipset_setup	= intel_845_setup
	},
	{
		.device_id	= PCI_DEVICE_ID_INTEL_82845G_HB,
		.chipset	= INTEL_I845_G,
		.chipset_name	= "i845G",
		.chipset_setup	= intel_845_setup
	},
	{
		.device_id	= PCI_DEVICE_ID_INTEL_82850_HB,
		.chipset	= INTEL_I850,
		.chipset_name	= "i850",
		.chipset_setup	= intel_850_setup
	},
	{
		.device_id	= PCI_DEVICE_ID_INTEL_82860_HB,
		.chipset	= INTEL_I860,
		.chipset_name	= "i860",
		.chipset_setup	= intel_860_setup
	},
	{ }, /* dummy final entry, always present */
};


/* scan table above for supported devices */
static int __init agp_lookup_host_bridge (struct pci_dev *pdev)
{
	int j=0;
	struct agp_device_ids *devs;
	
	devs = intel_agp_device_ids;

	while (devs[j].chipset_name != NULL) {
		if (pdev->device == devs[j].device_id) {
			printk (KERN_INFO PFX "Detected Intel %s chipset\n",
				devs[j].chipset_name);
			agp_bridge.type = devs[j].chipset;

			if (devs[j].chipset_setup != NULL)
				return devs[j].chipset_setup(pdev);
			else
				return intel_generic_setup(pdev);
		}
		j++;
	}
	j--;

	/* try init anyway, if user requests it */
	if (agp_try_unsupported) {
		printk(KERN_WARNING PFX "Trying generic Intel routines"
			" for device id: %04x\n", pdev->device);
		agp_bridge.type = INTEL_GENERIC;
		return intel_generic_setup(pdev);
	}

	printk(KERN_ERR PFX "Unsupported Intel chipset (device id: %04x),"
		" you might want to try agp_try_unsupported=1.\n", pdev->device);
	return -ENODEV;
}


/* Supported Device Scanning routine */

static int __init agp_find_supported_device(struct pci_dev *dev)
{
	struct pci_dev *i810_dev;
	u8 cap_ptr = 0;

	agp_bridge.dev = dev;

	/* This shit needs moving into tables/init-routines. */
	switch (dev->device) {
	case PCI_DEVICE_ID_INTEL_82810_MC1:
		i810_dev = pci_find_device(PCI_VENDOR_ID_INTEL, PCI_DEVICE_ID_INTEL_82810_IG1, NULL);
		if (i810_dev == NULL) {
			printk(KERN_ERR PFX "Detected an Intel i810,"
				" but could not find the secondary device.\n");
			return -ENODEV;
		}
		printk(KERN_INFO PFX "Detected an Intel i810 Chipset.\n");
		agp_bridge.type = INTEL_I810;
		return intel_i810_setup (i810_dev);

	case PCI_DEVICE_ID_INTEL_82810_MC3:
		i810_dev = pci_find_device(PCI_VENDOR_ID_INTEL, PCI_DEVICE_ID_INTEL_82810_IG3, NULL);
		if (i810_dev == NULL) {
			printk(KERN_ERR PFX "Detected an Intel i810 DC100, but could not find the "
				"secondary device.\n");
			return -ENODEV;
		}
		printk(KERN_INFO PFX "Detected an Intel i810 DC100 Chipset.\n");
		agp_bridge.type = INTEL_I810;
		return intel_i810_setup(i810_dev);

	case PCI_DEVICE_ID_INTEL_82810E_MC:
		i810_dev = pci_find_device(PCI_VENDOR_ID_INTEL, PCI_DEVICE_ID_INTEL_82810E_IG, NULL);
		if (i810_dev == NULL) {
			printk(KERN_ERR PFX "Detected an Intel i810 E"
				", but could not find the secondary device.\n");
			return -ENODEV;
		}
		printk(KERN_INFO PFX "Detected an Intel i810 E Chipset.\n");
		agp_bridge.type = INTEL_I810;
		return intel_i810_setup(i810_dev);

	 case PCI_DEVICE_ID_INTEL_82815_MC:
		/* The i815 can operate either as an i810 style
		 * integrated device, or as an AGP4X motherboard.
		 *
		 * This only addresses the first mode:
		 */
		i810_dev = pci_find_device(PCI_VENDOR_ID_INTEL, PCI_DEVICE_ID_INTEL_82815_CGC, NULL);
		if (i810_dev == NULL) {
			printk(KERN_ERR PFX "agpgart: Detected an "
				"Intel i815, but could not find the"
				" secondary device. Assuming a "
				"non-integrated video card.\n");
			break;
		}
		printk(KERN_INFO PFX "agpgart: Detected an Intel i815 Chipset.\n");
		agp_bridge.type = INTEL_I810;
		return intel_i810_setup(i810_dev);

	case PCI_DEVICE_ID_INTEL_82845G_HB:
		i810_dev = pci_find_device(PCI_VENDOR_ID_INTEL,
				PCI_DEVICE_ID_INTEL_82845G_IG, NULL);
		if (i810_dev && PCI_FUNC(i810_dev->devfn) != 0) {
			i810_dev = pci_find_device(PCI_VENDOR_ID_INTEL,
				PCI_DEVICE_ID_INTEL_82845G_IG, i810_dev);
		}

		if (i810_dev == NULL) {
			/* 
			 * We probably have a I845MP chipset with an external graphics
			 * card. It will be initialized later 
			 */
			agp_bridge.type = INTEL_I845_G;
			break;
		}
		printk(KERN_INFO PFX "Detected an Intel 845G Chipset.\n");
		agp_bridge.type = INTEL_I810;
		return intel_i830_setup(i810_dev);

	case PCI_DEVICE_ID_INTEL_82830_HB:
		i810_dev = pci_find_device(PCI_VENDOR_ID_INTEL, PCI_DEVICE_ID_INTEL_82830_CGC, NULL);
		if(i810_dev && PCI_FUNC(i810_dev->devfn) != 0)
			i810_dev = pci_find_device(PCI_VENDOR_ID_INTEL, PCI_DEVICE_ID_INTEL_82830_CGC, i810_dev);

		if (i810_dev == NULL) {
			/* Intel 830MP with external graphic card */
			/* It will be initialized later */
			agp_bridge.type = INTEL_I830_M;
			break;
		}
		printk(KERN_INFO PFX "Detected an Intel 830M Chipset.\n");
		agp_bridge.type = INTEL_I810;
		return intel_i830_setup(i810_dev);

	default:
		break;
	}

	cap_ptr = pci_find_capability(dev, PCI_CAP_ID_AGP);
	if (cap_ptr == 0)
		return -ENODEV;
	agp_bridge.capndx = cap_ptr;

	/* Fill in the mode register */
	pci_read_config_dword(agp_bridge.dev, agp_bridge.capndx+PCI_AGP_STATUS, &agp_bridge.mode);

	/* probe for known chipsets */
	return agp_lookup_host_bridge(dev);
}

static struct agp_driver intel_agp_driver = {
	.owner = THIS_MODULE,
};

static int __init agp_intel_probe (struct pci_dev *dev, const struct pci_device_id *ent)
{
	if (agp_find_supported_device(dev) == 0) {
		intel_agp_driver.dev = dev;	
		agp_register_driver(&intel_agp_driver);
		return 0;
	}
	return -ENODEV;	
}

static struct pci_device_id agp_intel_pci_table[] __initdata = {
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

MODULE_DEVICE_TABLE(pci, agp_intel_pci_table);

static struct __initdata pci_driver agp_intel_pci_driver = {
	.name		= "agpgart-intel",
	.id_table	= agp_intel_pci_table,
	.probe		= agp_intel_probe,
};

/* intel_agp_init() must not be declared static for explicit
   early initialization to work (ie i810fb) */
int __init agp_intel_init(void)
{
	int ret_val;
	static int agp_initialised=0;

	if (agp_initialised==1)
		return 0;
	agp_initialised=1;

	ret_val = pci_module_init(&agp_intel_pci_driver);
	if (ret_val)
		agp_bridge.type = NOT_SUPPORTED;

	return ret_val;
}

static void __exit agp_intel_cleanup(void)
{
	agp_unregister_driver(&intel_agp_driver);
	pci_unregister_driver(&agp_intel_pci_driver);
}

module_init(agp_intel_init);
module_exit(agp_intel_cleanup);

MODULE_PARM(agp_try_unsupported, "1i");
MODULE_AUTHOR("Dave Jones <davej@codemonkey.org.uk>");
MODULE_LICENSE("GPL and additional rights");
