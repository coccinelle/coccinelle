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
#if 0
#define PSEUDO_DMA
#endif

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
#include <asm/irq.h>
#include <asm/system.h>

#include <asm/macintosh.h>
#include <asm/macints.h>
#include <asm/machw.h>
#include <asm/mac_via.h>

#include "scsi.h"
#include "hosts.h"
#include "mac_scsi.h"
#include "NCR5380.h"
#include "constants.h"

#if 0
#define NDEBUG (NDEBUG_INTR | NDEBUG_PSEUDO_DMA | NDEBUG_ARBITRATION | NDEBUG_SELECTION | NDEBUG_RESELECTION)
#else
#define NDEBUG (NDEBUG_ABORT)
#endif

#define RESET_BOOT
#define DRIVER_SETUP

/*
 * BUG can be used to trigger a strange code-size related hang on 2.1 kernels
 */
#ifdef BUG
#undef RESET_BOOT
#undef DRIVER_SETUP
#endif

#define	ENABLE_IRQ()	mac_enable_irq( IRQ_MAC_SCSI ); 
#define	DISABLE_IRQ()	mac_disable_irq( IRQ_MAC_SCSI );

#ifdef RESET_BOOT
static void mac_scsi_reset_boot(struct Scsi_Host *instance);
#endif
static char macscsi_read(struct Scsi_Host *instance, int reg);
static void macscsi_write(struct Scsi_Host *instance, int reg, int value);

static int setup_can_queue = -1;
static int setup_cmd_per_lun = -1;
static int setup_sg_tablesize = -1;
#ifdef SUPPORT_TAGS
static int setup_use_tagged_queuing = -1;
#endif
static int setup_hostid = -1;

static int polled_scsi_on = 0;

/* Time (in jiffies) to wait after a reset; the SCSI standard calls for 250ms,
 * we usually do 0.5s to be on the safe side. But Toshiba CD-ROMs once more
 * need ten times the standard value... */
#define TOSHIBA_DELAY

#ifdef TOSHIBA_DELAY
#define	AFTER_RESET_DELAY	(5*HZ/2)
#else
#define	AFTER_RESET_DELAY	(HZ/2)
#endif

static volatile unsigned char *mac_scsi_regp = NULL;
static volatile unsigned char *mac_scsi_drq  = NULL;
static volatile unsigned char *mac_scsi_nodrq = NULL;

/*
 * Function : mac_scsi_setup(char *str, int *ints)
 *
 * Purpose : booter command line initialization of the overrides array,
 *
 * Inputs : str - unused, ints - array of integer parameters with ints[0]
 *	equal to the number of ints.
 *
 */

static int __init mac_scsi_setup(char *str, int *ints) {
#ifdef DRIVER_SETUP
	/* Format of mac5380 parameter is:
	 *   mac5380=<can_queue>,<cmd_per_lun>,<sg_tablesize>,<hostid>,<use_tags>
	 * Negative values mean don't change.
	 */
	
	/* Grmbl... the standard parameter parsing can't handle negative numbers
	 * :-( So let's do it ourselves!
	 */

	int i = ints[0]+1, fact;

	while( str && (isdigit(*str) || *str == '-') && i <= 10) {
		if (*str == '-')
			fact = -1, ++str;
		else
			fact = 1;
		ints[i++] = simple_strtoul( str, NULL, 0 ) * fact;
		if ((str = strchr( str, ',' )) != NULL)
			++str;
	}
	ints[0] = i-1;
	
	if (ints[0] < 1) {
		printk( "mac_scsi_setup: no arguments!\n" );
		return 0;
	}

	if (ints[0] >= 1) {
		if (ints[1] > 0)
			/* no limits on this, just > 0 */
			setup_can_queue = ints[1];
	}
	if (ints[0] >= 2) {
		if (ints[2] > 0)
			setup_cmd_per_lun = ints[2];
	}
	if (ints[0] >= 3) {
		if (ints[3] >= 0) {
			setup_sg_tablesize = ints[3];
			/* Must be <= SG_ALL (255) */
			if (setup_sg_tablesize > SG_ALL)
				setup_sg_tablesize = SG_ALL;
		}
	}
	if (ints[0] >= 4) {
		/* Must be between 0 and 7 */
		if (ints[4] >= 0 && ints[4] <= 7)
			setup_hostid = ints[4];
		else if (ints[4] > 7)
			printk( "mac_scsi_setup: invalid host ID %d !\n", ints[4] );
	}
#ifdef SUPPORT_TAGS
	if (ints[0] >= 5) {
		if (ints[5] >= 0)
			setup_use_tagged_queuing = !!ints[5];
	}
#endif
#endif
	return 1; 
}

__setup("mac5380=", mac_scsi_setup);

#if 0
#define MAC_ADDRESS(card) (ecard_address((card), ECARD_IOC, ECARD_SLOW) + 0x800)
#define MAC_IRQ(card)     ((card)->irq)
#endif

/*
 * XXX: status debug
 */
static struct Scsi_Host *default_instance;

/*
 * Function : int macscsi_detect(Scsi_Host_Template * tpnt)
 *
 * Purpose : initializes mac NCR5380 driver based on the
 *	command line / compile time port and irq definitions.
 *
 * Inputs : tpnt - template for this SCSI adapter.
 *
 * Returns : 1 if a host adapter was found, 0 if not.
 *
 */
 
int macscsi_detect(Scsi_Host_Template * tpnt)
{
    int count = 0;
    static int called = 0;
    struct Scsi_Host *instance;

    if (!MACH_IS_MAC || called)
	return( 0 );

    if (macintosh_config->scsi_type != MAC_SCSI_OLD)
	 return( 0 );

    tpnt->proc_name = "mac5380";

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

#ifdef SUPPORT_TAGS
	if (setup_use_tagged_queuing < 0)
		setup_use_tagged_queuing = DEFAULT_USE_TAGGED_QUEUING;
#endif

#if 0	/* loop over multiple adapters (Powerbooks ??) */
    for (count = 0; count < mac_num_scsi; count++) {
#endif
        instance = scsi_register (tpnt, sizeof(struct NCR5380_hostdata));
        if(instance == NULL)
        	continue;
	default_instance = instance;

	if (macintosh_config->ident == MAC_MODEL_IIFX) {
		mac_scsi_regp  = via1+0x8000;
		mac_scsi_drq   = via1+0xE000;
		mac_scsi_nodrq = via1+0xC000;
	} else {
		mac_scsi_regp  = via1+0x10000;
		mac_scsi_drq   = via1+0x6000;
		mac_scsi_nodrq = via1+0x12000;
	}


        instance->io_port = (unsigned long) mac_scsi_regp;
	instance->irq = IRQ_MAC_SCSI;

#ifdef RESET_BOOT
	mac_scsi_reset_boot(instance);
#endif

	NCR5380_init(instance, 0);

	instance->n_io_port = 255;

        ((struct NCR5380_hostdata *)instance->hostdata)->ctrl = 0;

	if (instance->irq != IRQ_NONE)
	    if (request_irq(instance->irq, NCR5380_intr, IRQ_FLG_SLOW, "ncr5380", NCR5380_intr)) {
		printk("scsi%d: IRQ%d not free, interrupts disabled\n",
		    instance->host_no, instance->irq);
		instance->irq = IRQ_NONE;
	    }

	printk("scsi%d: generic 5380 at port %lX irq", instance->host_no, instance->io_port);
	if (instance->irq == IRQ_NONE)
	    printk ("s disabled");
	else
	    printk (" %d", instance->irq);
	printk(" options CAN_QUEUE=%d CMD_PER_LUN=%d release=%d",
	    instance->can_queue, instance->cmd_per_lun, MACSCSI_PUBLIC_RELEASE);
	printk("\nscsi%d:", instance->host_no);
	NCR5380_print_options(instance);
	printk("\n");
#if 0	/* multiple adapters */
    }
#endif
    called = 1;
    return 1;

}

int macscsi_release (struct Scsi_Host *shpnt)
{
	if (shpnt->irq != IRQ_NONE)
		free_irq (shpnt->irq, NCR5380_intr);

	return 0;
}

#ifdef RESET_BOOT
/*
 * Our 'bus reset on boot' function
 */

static void mac_scsi_reset_boot(struct Scsi_Host *instance)
{
	unsigned long end;

	NCR5380_local_declare();
	NCR5380_setup(instance);
	
	/*
	 * Do a SCSI reset to clean up the bus during initialization. No messing
	 * with the queues, interrupts, or locks necessary here.
	 */

	printk( "Macintosh SCSI: resetting the SCSI bus..." );

	/* switch off SCSI IRQ - catch an interrupt without IRQ bit set else */
       	mac_disable_irq(IRQ_MAC_SCSI);

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
       	mac_enable_irq(IRQ_MAC_SCSI);

	printk( " done\n" );
}
#endif

const char * macscsi_info (struct Scsi_Host *spnt) {
    return "";
}

void restore_irq(struct pt_regs *regs)
{
	unsigned long flags;

	save_flags(flags);
	flags = (flags & ~0x0700) | (regs->sr & 0x0700);
	restore_flags(flags);
}

/*
 * pseudo-DMA transfer functions, copied and modified from Russel King's
 * ARM 5380 driver (cumana_1)
 *
 * Work in progress (sort of), didn't work last time I checked, don't use!
 */

#ifdef NOT_EFFICIENT
#define CTRL(p,v)     outb(*ctrl = (v), (p) - 577)
#define STAT(p)       inb((p)+1)
#define IN(p)         inb((p))
#define OUT(v,p)      outb((v), (p))
#else
#if 0
#define CTRL(p,v)	(p[-2308] = (*ctrl = (v)))
#else
#define CTRL(p,v)	(*ctrl = (v))
#endif
#define STAT(p)		(p[1<<4])
#define IN(p)		(*(p))
#define IN2(p)		((unsigned short)(*(volatile unsigned long *)(p)))
#define OUT(v,p)	(*(p) = (v))
#define OUT2(v,p)	(*((volatile unsigned long *)(p)) = (v))
#endif
#define L(v)		(((v)<<16)|((v) & 0x0000ffff))
#define H(v)		(((v)>>16)|((v) & 0xffff0000))
#define ioaddr(v)	(v)

static inline int NCR5380_pwrite(struct Scsi_Host *instance, unsigned char *addr,
              int len)
{
  int *ctrl = &((struct NCR5380_hostdata *)instance->hostdata)->ctrl;
  int oldctrl = *ctrl;
  unsigned long *laddr;
#ifdef NOT_EFFICIENT
  int iobase = instance->io_port;
  int dma_io = mac_scsi_nodrq;
#else
  volatile unsigned char *iobase = (unsigned char *)ioaddr(instance->io_port);
  volatile unsigned char *dma_io = (unsigned char *)(mac_scsi_nodrq);
#endif

  if(!len) return 0;

  CTRL(iobase, 0x02);
  laddr = (unsigned long *)addr;
  while(len >= 32)
  {
    int status;
    unsigned long v;
    status = STAT(iobase);
    if(status & 0x80)
      goto end;
    if(!(status & 0x40))
      continue;
    v=*laddr++; OUT2(L(v),dma_io); OUT2(H(v),dma_io);
    v=*laddr++; OUT2(L(v),dma_io); OUT2(H(v),dma_io);
    v=*laddr++; OUT2(L(v),dma_io); OUT2(H(v),dma_io);
    v=*laddr++; OUT2(L(v),dma_io); OUT2(H(v),dma_io);
    v=*laddr++; OUT2(L(v),dma_io); OUT2(H(v),dma_io);
    v=*laddr++; OUT2(L(v),dma_io); OUT2(H(v),dma_io);
    v=*laddr++; OUT2(L(v),dma_io); OUT2(H(v),dma_io);
    v=*laddr++; OUT2(L(v),dma_io); OUT2(H(v),dma_io);
    len -= 32;
    if(len == 0)
      break;
  }

  addr = (unsigned char *)laddr;
  CTRL(iobase, 0x12);
  while(len > 0)
  {
    int status;
    status = STAT(iobase);
    if(status & 0x80)
      goto end;
    if(status & 0x40)
    {
      OUT(*addr++, dma_io);
      if(--len == 0)
        break;
    }

    status = STAT(iobase);
    if(status & 0x80)
      goto end;
    if(status & 0x40)
    {
      OUT(*addr++, dma_io);
      if(--len == 0)
        break;
    }
  }
end:
  CTRL(iobase, oldctrl|0x40);
  return len;
}

static inline int NCR5380_pread(struct Scsi_Host *instance, unsigned char *addr,
              int len)
{
  int *ctrl = &((struct NCR5380_hostdata *)instance->hostdata)->ctrl;
  int oldctrl = *ctrl;
  unsigned long *laddr;
#ifdef NOT_EFFICIENT
  int iobase = instance->io_port;
  int dma_io = mac_scsi_nodrq;
#else
  volatile unsigned char *iobase = (unsigned char *)ioaddr(instance->io_port);
  volatile unsigned char *dma_io = (unsigned char *)((int)mac_scsi_nodrq);
#endif

  if(!len) return 0;

  CTRL(iobase, 0x00);
  laddr = (unsigned long *)addr;
  while(len >= 32)
  {
    int status;
    status = STAT(iobase);
    if(status & 0x80)
      goto end;
    if(!(status & 0x40))
      continue;
    *laddr++ = IN2(dma_io)|(IN2(dma_io)<<16);
    *laddr++ = IN2(dma_io)|(IN2(dma_io)<<16);
    *laddr++ = IN2(dma_io)|(IN2(dma_io)<<16);
    *laddr++ = IN2(dma_io)|(IN2(dma_io)<<16);
    *laddr++ = IN2(dma_io)|(IN2(dma_io)<<16);
    *laddr++ = IN2(dma_io)|(IN2(dma_io)<<16);
    *laddr++ = IN2(dma_io)|(IN2(dma_io)<<16);
    *laddr++ = IN2(dma_io)|(IN2(dma_io)<<16);
    len -= 32;
    if(len == 0)
      break;
  }

  addr = (unsigned char *)laddr;
  CTRL(iobase, 0x10);
  while(len > 0)
  {
    int status;
    status = STAT(iobase);
    if(status & 0x80)
      goto end;
    if(status & 0x40)
    {
      *addr++ = IN(dma_io);
      if(--len == 0)
        break;
    }

    status = STAT(iobase);
    if(status & 0x80)
      goto end;
    if(status & 0x40)
    {
      *addr++ = IN(dma_io);
      if(--len == 0)
        break;
    }
  }
end:
  CTRL(iobase, oldctrl|0x40);
  return len;
}

#undef STAT
#undef CTRL
#undef IN
#undef OUT

/*
 * NCR 5380 register access functions
 */

#ifdef ORIG
#if 0
#define CTRL(p,v) outb(*ctrl = (v), (p) - 577)
#else
#define CTRL(p,v) (*ctrl = (v))
#endif

static char macscsi_read(struct Scsi_Host *instance, int reg)
{
  int iobase = instance->io_port;
  int i;
  int *ctrl = &((struct NCR5380_hostdata *)instance->hostdata)->ctrl;

  CTRL(iobase, 0);
#if 0
  i = inb(iobase + 64 + reg);
#else
  i = inb(iobase + reg<<4);
#endif
  CTRL(iobase, 0x40);

  return i;
}

static void macscsi_write(struct Scsi_Host *instance, int reg, int value)
{
  int iobase = instance->io_port;
  int *ctrl = &((struct NCR5380_hostdata *)instance->hostdata)->ctrl;

  CTRL(iobase, 0);
#if 0
  outb(value, iobase + 64 + reg);
#else
  outb(value, iobase + reg<<4);
#endif
  CTRL(iobase, 0x40);
}

#undef CTRL

#else
static char macscsi_read(struct Scsi_Host *instance, int reg)
{
	return( mac_scsi_regp[reg << 4] );
}

static void macscsi_write(struct Scsi_Host *instance, int reg, int value)
{
	mac_scsi_regp[reg << 4] = value;
}

#endif

#include "NCR5380.c"

/*
 * Debug stuff - to be called on NMI, or sysrq key. Use at your own risk; 
 * reentering NCR5380_print_status seems to have ugly side effects
 */

void scsi_mac_debug (void)
{
	unsigned long flags;
	NCR5380_local_declare();

	if (default_instance) {
#if 0
		NCR5380_setup(default_instance);
		if(NCR5380_read(BUS_AND_STATUS_REG)&BASR_IRQ)
#endif
			save_flags(flags);
			cli();
			NCR5380_print_status(default_instance);
			restore_flags(flags);
	}
#if 0
	polled_scsi_on = 1;
#endif
}
/*
 * Helper function for interrupt trouble. More ugly side effects here.
 */

void scsi_mac_polled (void)
{
	unsigned long flags;
	NCR5380_local_declare();
	struct Scsi_Host *instance;
	
#if 0
	for (instance = first_instance; instance && (instance->hostt == 
	    the_template); instance = instance->next)
	    if (instance->irq == IRQ_MAC_SCSI && polled_scsi_on) {
#else
		instance = default_instance;
#endif
		NCR5380_setup(instance);
		if(NCR5380_read(BUS_AND_STATUS_REG)&BASR_IRQ)
		{
			printk("SCSI poll\n");
			save_flags(flags);
			cli();
			NCR5380_intr(IRQ_MAC_SCSI, instance, NULL);
			restore_flags(flags);
		}
#if 0
	    }
#endif
}



static Scsi_Host_Template driver_template = MAC_NCR5380;

#include "scsi_module.c"
