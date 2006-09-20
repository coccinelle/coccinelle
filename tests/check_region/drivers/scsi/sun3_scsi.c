/*
 * Sun3 SCSI stuff by Erik Verbruggen (erik@bigmama.xtdnet.nl)
 *
 * Sun3 DMA routines added by Sam Creasey (sammy@oh.verio.com)
 *
 * Adapted from mac_scsinew.c:
 */
/*
 * Generic Macintosh NCR5380 driver
 *
 * Copyright 1998, Michael Schmitz <mschmitz@lbl.gov>
 *
 * derived in part from:
 */
/*
 * Generic Generic NCR5380 driver
 *
 * Copyright 1995, Russell King
 *
 * ALPHA RELEASE 1.
 *
 * For more information, please consult
 *
 * NCR 5380 Family
 * SCSI Protocol Controller
 * Databook
 *
 * NCR Microelectronics
 * 1635 Aeroplaza Drive
 * Colorado Springs, CO 80916
 * 1+ (719) 578-3400
 * 1+ (800) 334-5454
 */


/*
 * This is from mac_scsi.h, but hey, maybe this is usefull for Sun3 too! :)
 *
 * Options :
 *
 * PARITY - enable parity checking.  Not supported.
 *
 * SCSI2 - enable support for SCSI-II tagged queueing.  Untested.
 *
 * USLEEP - enable support for devices that don't disconnect.  Untested.
 */

/*
 * $Log$
 */

#define AUTOSENSE

#include <linux/types.h>
#include <linux/stddef.h>
#include <linux/ctype.h>
#include <linux/delay.h>

#include <linux/module.h>
#include <linux/signal.h>
#include <linux/sched.h>
#include <linux/ioport.h>
#include <linux/init.h>
#include <linux/blk.h>

#include <asm/io.h>
#include <asm/system.h>

#include <asm/sun3ints.h>
#include <asm/dvma.h>
/* dma on! */
#define REAL_DMA

#include "scsi.h"
#include "hosts.h"
#include "sun3_scsi.h"
#include "NCR5380.h"
#include "constants.h"

#define USE_WRAPPER
#define RESET_BOOT
#define DRIVER_SETUP

#define NDEBUG 0

/*
 * BUG can be used to trigger a strange code-size related hang on 2.1 kernels
 */
#ifdef BUG
#undef RESET_BOOT
#undef DRIVER_SETUP
#endif

#undef SUPPORT_TAGS

#define	ENABLE_IRQ()	enable_irq( IRQ_SUN3_SCSI ); 


static void scsi_sun3_intr(int irq, void *dummy, struct pt_regs *fp);
static inline unsigned char sun3scsi_read(int reg);
static inline void sun3scsi_write(int reg, int value);

static int setup_can_queue = -1;
static int setup_cmd_per_lun = -1;
static int setup_sg_tablesize = -1;
#ifdef SUPPORT_TAGS
static int setup_use_tagged_queuing = -1;
#endif
static int setup_hostid = -1;

static Scsi_Cmnd *sun3_dma_setup_done = NULL;

#define	AFTER_RESET_DELAY	(HZ/2)

/* ms to wait after hitting dma regs */
#define SUN3_DMA_DELAY 5

/* dvma buffer to allocate -- 32k should hopefully be more than sufficient */
#define SUN3_DVMA_BUFSIZE 0xe000

/* minimum number of bytes to to dma on */
#define SUN3_DMA_MINSIZE 128

static volatile unsigned char *sun3_scsi_regp;
static volatile struct sun3_dma_regs *dregs;
static unsigned char *dmabuf = NULL; /* dma memory buffer */
static struct sun3_udc_regs *udc_regs = NULL;
static void *sun3_dma_orig_addr = NULL;
static unsigned long sun3_dma_orig_count = 0;
static int sun3_dma_active = 0;

/*
 * NCR 5380 register access functions
 */

static inline unsigned char sun3scsi_read(int reg)
{
	return( sun3_scsi_regp[reg] );
}

static inline void sun3scsi_write(int reg, int value)
{
	sun3_scsi_regp[reg] = value;
}

/* dma controller register access functions */

static inline unsigned short sun3_udc_read(unsigned char reg)
{
	unsigned short ret;

	dregs->udc_addr = UDC_CSR;
	udelay(SUN3_DMA_DELAY);
	ret = dregs->udc_data;
	udelay(SUN3_DMA_DELAY);
	
	return ret;
}

static inline void sun3_udc_write(unsigned short val, unsigned char reg)
{
	dregs->udc_addr = reg;
	udelay(SUN3_DMA_DELAY);
	dregs->udc_data = val;
	udelay(SUN3_DMA_DELAY);
}

/*
 * XXX: status debug
 */
static struct Scsi_Host *default_instance;

/*
 * Function : int sun3scsi_detect(Scsi_Host_Template * tpnt)
 *
 * Purpose : initializes mac NCR5380 driver based on the
 *	command line / compile time port and irq definitions.
 *
 * Inputs : tpnt - template for this SCSI adapter.
 *
 * Returns : 1 if a host adapter was found, 0 if not.
 *
 */
 
int sun3scsi_detect(Scsi_Host_Template * tpnt)
{
	unsigned long ioaddr, iopte;
	int count = 0;
	static int called = 0;
	struct Scsi_Host *instance;

	if(called)
		return 0;

	tpnt->proc_name = "Sun3 5380 SCSI";

	/* setup variables */
	tpnt->can_queue =
		(setup_can_queue > 0) ? setup_can_queue : CAN_QUEUE;
	tpnt->cmd_per_lun =
		(setup_cmd_per_lun > 0) ? setup_cmd_per_lun : CMD_PER_LUN;
	tpnt->sg_tablesize = 
		(setup_sg_tablesize >= 0) ? setup_sg_tablesize : SG_TABLESIZE;

	if (setup_hostid >= 0)
		tpnt->this_id = setup_hostid;
	else {
		/* use 7 as default */
		tpnt->this_id = 7;
	}

	/* Taken from Sammy's lance driver: */
        /* IOBASE_SUN3_SCSI can be found within the IO pmeg with some effort */
        for(ioaddr = 0xfe00000; ioaddr < (0xfe00000 + SUN3_PMEG_SIZE);
            ioaddr += SUN3_PTE_SIZE) {

                iopte = sun3_get_pte(ioaddr);
                if(!(iopte & SUN3_PAGE_TYPE_IO)) /* this an io page? */
                        continue;

                if(((iopte & SUN3_PAGE_PGNUM_MASK) << PAGE_SHIFT) ==
                   IOBASE_SUN3_SCSI) {
                        count = 1;
                        break;
                }
        }

	if(!count) {
		printk("No Sun3 NCR5380 found!\n");
		return 0;
	}

	sun3_scsi_regp = (unsigned char *)ioaddr;
	dregs = (struct sun3_dma_regs *)(((unsigned char *)ioaddr) + 8);

	if((dmabuf = sun3_dvma_malloc(SUN3_DVMA_BUFSIZE)) == NULL) {
	     printk("SUN3 Scsi couldn't allocate DVMA memory!\n");
	     return 0;
	}

	if((udc_regs = sun3_dvma_malloc(sizeof(struct sun3_udc_regs)))
	   == NULL) {
	     printk("SUN3 Scsi couldn't allocate DVMA memory!\n");
	     return 0;
	}

#ifdef SUPPORT_TAGS
	if (setup_use_tagged_queuing < 0)
		setup_use_tagged_queuing = DEFAULT_USE_TAGGED_QUEUING;
#endif

	instance = scsi_register (tpnt, sizeof(struct NCR5380_hostdata));
	if(instance == NULL)
		return 0;
		
	default_instance = instance;

        instance->io_port = (unsigned long) ioaddr;
	instance->irq = IRQ_SUN3_SCSI;

	NCR5380_init(instance, 0);

	instance->n_io_port = 32;

        ((struct NCR5380_hostdata *)instance->hostdata)->ctrl = 0;

	if (request_irq(instance->irq, scsi_sun3_intr,
			     0, "Sun3SCSI-5380", NULL)) {
#ifndef REAL_DMA
		printk("scsi%d: IRQ%d not free, interrupts disabled\n",
		       instance->host_no, instance->irq);
		instance->irq = IRQ_NONE;
#else
		printk("scsi%d: IRQ%d not free, bailing out\n",
		       instance->host_no, instance->irq);
		return 0;
#endif
	}
	
	printk("scsi%d: Sun3 5380 at port %lX irq", instance->host_no, instance->io_port);
	if (instance->irq == IRQ_NONE)
		printk ("s disabled");
	else
		printk (" %d", instance->irq);
	printk(" options CAN_QUEUE=%d CMD_PER_LUN=%d release=%d",
	       instance->can_queue, instance->cmd_per_lun,
	       SUN3SCSI_PUBLIC_RELEASE);
	printk("\nscsi%d:", instance->host_no);
	NCR5380_print_options(instance);
	printk("\n");

	dregs->csr = 0;
	udelay(SUN3_DMA_DELAY);
	dregs->csr = CSR_SCSI | CSR_FIFO | CSR_INTR;
	udelay(SUN3_DMA_DELAY);
	dregs->fifo_count = 0;

	called = 1;
	return 1;
}

int sun3scsi_release (struct Scsi_Host *shpnt)
{
	if (shpnt->irq != IRQ_NONE)
		free_irq (shpnt->irq, NULL);

	return 0;
}

#ifdef RESET_BOOT
/*
 * Our 'bus reset on boot' function
 */

static void sun3_scsi_reset_boot(struct Scsi_Host *instance)
{
	unsigned long end;

	NCR5380_local_declare();
	NCR5380_setup(instance);
	
	/*
	 * Do a SCSI reset to clean up the bus during initialization. No
	 * messing with the queues, interrupts, or locks necessary here.
	 */

	printk( "Sun3 SCSI: resetting the SCSI bus..." );

	/* switch off SCSI IRQ - catch an interrupt without IRQ bit set else */
       	sun3_disable_irq( IRQ_SUN3_SCSI );

	/* get in phase */
	NCR5380_write( TARGET_COMMAND_REG,
		      PHASE_SR_TO_TCR( NCR5380_read(STATUS_REG) ));

	/* assert RST */
	NCR5380_write( INITIATOR_COMMAND_REG, ICR_BASE | ICR_ASSERT_RST );

	/* The min. reset hold time is 25us, so 40us should be enough */
	udelay( 50 );

	/* reset RST and interrupt */
	NCR5380_write( INITIATOR_COMMAND_REG, ICR_BASE );
	NCR5380_read( RESET_PARITY_INTERRUPT_REG );

	for( end = jiffies + AFTER_RESET_DELAY; jiffies < end; )
		barrier();

	/* switch on SCSI IRQ again */
       	sun3_enable_irq( IRQ_SUN3_SCSI );

	printk( " done\n" );
}
#endif

const char * sun3scsi_info (struct Scsi_Host *spnt) {
    return "";
}

// safe bits for the CSR
#define CSR_GOOD 0x060f

static void scsi_sun3_intr(int irq, void *dummy, struct pt_regs *fp)
{
	unsigned short csr = dregs->csr;

	if(csr & ~CSR_GOOD) {
		if(csr & CSR_DMA_BUSERR) {
			printk("scsi%d: bus error in dma\n", default_instance->host_no);
		}

		if(csr & CSR_DMA_CONFLICT) {
			printk("scsi%d: dma conflict\n", default_instance->host_no);
		}
	}

	if(csr & (CSR_SDB_INT | CSR_DMA_INT))
		NCR5380_intr(irq, dummy, fp);
}

/*
 * Debug stuff - to be called on NMI, or sysrq key. Use at your own risk; 
 * reentering NCR5380_print_status seems to have ugly side effects
 */

/* this doesn't seem to get used at all -- sam */
#if 0
void sun3_sun3_debug (void)
{
	unsigned long flags;
	NCR5380_local_declare();

	if (default_instance) {
			save_flags(flags);
			cli();
			NCR5380_print_status(default_instance);
			restore_flags(flags);
	}
}
#endif


/* sun3scsi_dma_setup() -- initialize the dma controller for a read/write */
static unsigned long sun3scsi_dma_setup(void *data, unsigned long count, int write_flag)
{
	if(write_flag) 
		memcpy(dmabuf, data, count);
	else {
		sun3_dma_orig_addr = data;
		sun3_dma_orig_count = count;
	}

	dregs->fifo_count = 0;
	sun3_udc_write(UDC_RESET, UDC_CSR);
	
	/* reset fifo */
	dregs->csr &= ~CSR_FIFO;
	dregs->csr |= CSR_FIFO;
	
	/* set direction */
	if(write_flag)
		dregs->csr |= CSR_SEND;
	else
		dregs->csr &= ~CSR_SEND;
	
	/* byte count for fifo */
	dregs->fifo_count = count;

	sun3_udc_write(UDC_RESET, UDC_CSR);
	
	/* reset fifo */
	dregs->csr &= ~CSR_FIFO;
	dregs->csr |= CSR_FIFO;
	

	if(dregs->fifo_count != count) { 
		printk("scsi%d: fifo_mismatch %04x not %04x\n",
		       default_instance->host_no, dregs->fifo_count,
		       (unsigned int) count);
		NCR5380_print(default_instance);
	}

	/* setup udc */
	udc_regs->addr_hi = ((sun3_dvma_vtop(dmabuf) & 0xff0000) >> 8);
	udc_regs->addr_lo = (sun3_dvma_vtop(dmabuf) & 0xffff);
	udc_regs->count = count/2; /* count in words */
	udc_regs->mode_hi = UDC_MODE_HIWORD;
	if(write_flag) {
		if(count & 1)
			udc_regs->count++;
		udc_regs->mode_lo = UDC_MODE_LSEND;
		udc_regs->rsel = UDC_RSEL_SEND;
	} else {
		udc_regs->mode_lo = UDC_MODE_LRECV;
		udc_regs->rsel = UDC_RSEL_RECV;
	}
	
	/* announce location of regs block */
	sun3_udc_write(((sun3_dvma_vtop(udc_regs) & 0xff0000) >> 8),
		       UDC_CHN_HI); 

	sun3_udc_write((sun3_dvma_vtop(udc_regs) & 0xffff), UDC_CHN_LO);

	/* set dma master on */
	sun3_udc_write(0xd, UDC_MODE);

	/* interrupt enable */
	sun3_udc_write(UDC_INT_ENABLE, UDC_CSR);
	
       	return count;

}

static inline unsigned long sun3scsi_dma_residual(struct Scsi_Host *instance)
{
	unsigned short resid;

	dregs->udc_addr = 0x32; 
	udelay(SUN3_DMA_DELAY);
	resid = dregs->udc_data;
	udelay(SUN3_DMA_DELAY);
	resid *= 2;

	return (unsigned long) resid;
}

static inline unsigned long sun3scsi_dma_xfer_len(unsigned long wanted, Scsi_Cmnd *cmd,
				    int write_flag)
{
	if((cmd->request.cmd == 0) || (cmd->request.cmd == 1))
 		return wanted;
	else
		return 0;
}

/* clean up after our dma is done */
static int sun3scsi_dma_finish(void)
{
	unsigned short count;
	int ret = 0;
	
	count = sun3scsi_dma_residual(default_instance);

	sun3_dma_active = 0;

	/* if we've finished a read, copy out the data we read */
 	if(sun3_dma_orig_addr) {
		/* check for residual bytes after dma end */
		if(count && (NCR5380_read(BUS_AND_STATUS_REG) &
			     (BASR_PHASE_MATCH | BASR_ACK))) {
			printk("scsi%d: sun3_scsi_finish: read overrun baby... ", default_instance->host_no);
			printk("basr now %02x\n", NCR5380_read(BUS_AND_STATUS_REG));
			ret = count;
		}
		
		/* copy in what we dma'd no matter what */
		memcpy(sun3_dma_orig_addr, dmabuf, sun3_dma_orig_count);
		sun3_dma_orig_addr = NULL;

	}
	
	sun3_udc_write(UDC_RESET, UDC_CSR);
	
	dregs->csr &= ~CSR_SEND;

	/* reset fifo */
	dregs->csr &= ~CSR_FIFO;
	dregs->csr |= CSR_FIFO;
	
	sun3_dma_setup_done = NULL;

	return ret;

}
	
#include "sun3_NCR5380.c"

static Scsi_Host_Template driver_template = SUN3_NCR5380;

#include "scsi_module.c"

