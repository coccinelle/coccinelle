/*
 * Detection routine for the NCR53c710 based Amiga SCSI Controllers for Linux.
 *  		Amiga MacroSystemUS WarpEngine SCSI controller.
 *		Amiga Technologies A4000T SCSI controller.
 *		Amiga Technologies/DKB A4091 SCSI controller.
 *
 * Written 1997 by Alan Hourihane <alanh@fairlite.demon.co.uk>
 * plus modifications of the 53c7xx.c driver to support the Amiga.
 */
#include <linux/types.h>
#include <linux/mm.h>
#include <linux/blk.h>
#include <linux/sched.h>
#include <linux/version.h>
#include <linux/config.h>
#include <linux/zorro.h>

#include <asm/setup.h>
#include <asm/page.h>
#include <asm/pgtable.h>
#include <asm/amigaints.h>
#include <asm/amigahw.h>
#include <asm/io.h>
#include <asm/irq.h>

#include "scsi.h"
#include "hosts.h"
#include "53c7xx.h"
#include "amiga7xx.h"

#include<linux/stat.h>

extern int ncr53c7xx_init (Scsi_Host_Template *tpnt, int board, int chip, 
			   u32 base, int io_port, int irq, int dma,
			   long long options, int clock);

int __init amiga7xx_detect(Scsi_Host_Template *tpnt)
{
    static unsigned char called = 0;
    int num = 0, clock;
    long long options;
    struct zorro_dev *z = NULL;
    unsigned long address;

    if (called || !MACH_IS_AMIGA)
	return 0;

    tpnt->proc_name = "Amiga7xx";

#ifdef CONFIG_A4000T_SCSI
    if (AMIGAHW_PRESENT(A4000_SCSI)) {
	address = 0xdd0040;
	if (request_mem_region(address, 0x1000, "ncr53c710")) { 
	    address = ZTWO_VADDR(address);
	    options = OPTION_MEMORY_MAPPED | OPTION_DEBUG_TEST1 |
		      OPTION_INTFLY | OPTION_SYNCHRONOUS |
		      OPTION_ALWAYS_SYNCHRONOUS | OPTION_DISCONNECT;
	    clock = 50000000;	/* 50MHz SCSI Clock */
	    ncr53c7xx_init(tpnt, 0, 710, address, 0, IRQ_AMIGA_PORTS, DMA_NONE,
			   options, clock);
	    num++;
	}
    }
#endif

    while ((z = zorro_find_device(ZORRO_WILDCARD, z))) {
	unsigned long address = z->resource.start;
	unsigned long size = z->resource.end-z->resource.start+1;
	switch (z->id) {
#ifdef CONFIG_BLZ603EPLUS_SCSI
	    case ZORRO_PROD_PHASE5_BLIZZARD_603E_PLUS:
		address = 0xf40000;
		if (request_mem_region(address, 0x1000, "ncr53c710")) {
		    address = ZTWO_VADDR(address);
		    options = OPTION_MEMORY_MAPPED | OPTION_DEBUG_TEST1 |
			      OPTION_INTFLY | OPTION_SYNCHRONOUS | 
			      OPTION_ALWAYS_SYNCHRONOUS | OPTION_DISCONNECT;
		    clock = 50000000;	/* 50MHz SCSI Clock */
		    ncr53c7xx_init(tpnt, 0, 710, address, 0, IRQ_AMIGA_PORTS,
				   DMA_NONE, options, clock);
		    num++;
		}
		break;
#endif

#ifdef CONFIG_WARPENGINE_SCSI
    	    case ZORRO_PROD_MACROSYSTEMS_WARP_ENGINE_40xx:
		if (request_mem_region(address+0x40000, 0x1000, "ncr53c710")) {
		    address = (unsigned long)ioremap(address, size);
		    options = OPTION_MEMORY_MAPPED | OPTION_DEBUG_TEST1 |
			      OPTION_INTFLY | OPTION_SYNCHRONOUS |
			      OPTION_ALWAYS_SYNCHRONOUS | OPTION_DISCONNECT;
		    clock = 50000000;	/* 50MHz SCSI Clock */
		    ncr53c7xx_init(tpnt, 0, 710, address+0x40000, 0,
				   IRQ_AMIGA_PORTS, DMA_NONE, options, clock);
		    num++;
		}
		break;
#endif

#ifdef CONFIG_A4091_SCSI
	    case ZORRO_PROD_CBM_A4091_1:
	    case ZORRO_PROD_CBM_A4091_2:
		if (request_mem_region(address+0x800000, 0x1000, "ncr53c710")) {
		    address = (unsigned long)ioremap(address, size);
		    options = OPTION_MEMORY_MAPPED | OPTION_DEBUG_TEST1 |
			      OPTION_INTFLY | OPTION_SYNCHRONOUS |
			      OPTION_ALWAYS_SYNCHRONOUS | OPTION_DISCONNECT;
		    clock = 50000000;	/* 50MHz SCSI Clock */
		    ncr53c7xx_init(tpnt, 0, 710, address+0x800000, 0,
				   IRQ_AMIGA_PORTS, DMA_NONE, options, clock);
		    num++;
		}
		break;
#endif

#ifdef CONFIG_GVP_TURBO_SCSI
    	    case ZORRO_PROD_GVP_GFORCE_040_060:
		if (request_mem_region(address+0x40000, 0x1000, "ncr53c710")) {
		    address = ZTWO_VADDR(address);
		    options = OPTION_MEMORY_MAPPED | OPTION_DEBUG_TEST1 |
			      OPTION_INTFLY | OPTION_SYNCHRONOUS |
			      OPTION_ALWAYS_SYNCHRONOUS | OPTION_DISCONNECT;
		    clock = 50000000;	/* 50MHz SCSI Clock */
		    ncr53c7xx_init(tpnt, 0, 710, address+0x40000, 0,
				   IRQ_AMIGA_PORTS, DMA_NONE, options, clock);
		    num++;
		}
#endif
	}
    }

    called = 1;
    return num;
}
