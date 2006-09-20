/*===================================================================
 *
 *                    Linux MegaRAID device driver
 *
 * Copyright 1999 American Megatrends Inc.
 *
 *              This program is free software; you can redistribute it and/or
 *              modify it under the terms of the GNU General Public License
 *              as published by the Free Software Foundation; either version
 *              2 of the License, or (at your option) any later version.
 *
 * Version : 1.07b
 * 
 * Description: Linux device driver for AMI MegaRAID controller
 *
 * Supported controllers: MegaRAID 418, 428, 438, 466, 762, 467, 490
 * 
 * History:
 *
 * Version 0.90:
 *     Original source contributed by Dell; integrated it into the kernel and
 *     cleaned up some things.  Added support for 438/466 controllers.
 *
 * Version 0.91:
 *     Aligned mailbox area on 16-byte boundry.
 *     Added schedule() at the end to properly clean up.
 *     Made improvements for conformity to linux driver standards.
 *
 * Version 0.92:
 *     Added support for 2.1 kernels.
 *         Reads from pci_dev struct, so it's not dependent on pcibios.
 *         Added some missing virt_to_bus() translations.
 *     Added support for SMP.
 *         Changed global cli()'s to spinlocks for 2.1, and simulated
 *          spinlocks for 2.0.
 *     Removed setting of SA_INTERRUPT flag when requesting Irq.
 *
 * Version 0.92ac:
 *     Small changes to the comments/formatting. Plus a couple of
 *      added notes. Returned to the authors. No actual code changes
 *      save printk levels.
 *     8 Oct 98        Alan Cox <alan.cox@linux.org>
 *
 *     Merged with 2.1.131 source tree.
 *     12 Dec 98       K. Baranowski <kgb@knm.org.pl>                          
 *
 * Version 0.93:
 *     Added support for vendor specific ioctl commands (0x80+xxh)
 *     Changed some fields in MEGARAID struct to better values.
 *     Added signature check for Rp controllers under 2.0 kernels
 *     Changed busy-wait loop to be time-based
 *     Fixed SMP race condition in isr
 *     Added kfree (sgList) on release
 *     Added #include linux/version.h to megaraid.h for hosts.h
 *     Changed max_id to represent max logical drives instead of targets.
 *
 * Version 0.94:
 *     Got rid of some excess locking/unlocking
 *     Fixed slight memory corruption problem while memcpy'ing into mailbox
 *     Changed logical drives to be reported as luns rather than targets
 *     Changed max_id to 16 since it is now max targets/chan again.
 *     Improved ioctl interface for upcoming megamgr
 *
 * Version 0.95:
 *     Fixed problem of queueing multiple commands to adapter;
 *       still has some strange problems on some setups, so still
 *       defaults to single.  To enable parallel commands change
 *       #define MULTI_IO in megaraid.h
 *     Changed kmalloc allocation to be done in beginning.
 *     Got rid of C++ style comments
 *
 * Version 0.96:
 *     762 fully supported.
 *
 * Version 0.97:
 *     Changed megaraid_command to use wait_queue.
 *     Fixed bug of undesirably detecting HP onboard controllers which
 *       are disabled.
 *     
 * Version 1.00:
 *     Checks to see if an irq ocurred while in isr, and runs through
 *       routine again.
 *     Copies mailbox to temp area before processing in isr
 *     Added barrier() in busy wait to fix volatility bug
 *     Uses separate list for freed Scbs, keeps track of cmd state
 *     Put spinlocks around entire queue function for now...
 *     Full multi-io commands working stablely without previous problems
 *     Added skipXX LILO option for Madrona motherboard support
 *
 * Version 1.01:
 *     Fixed bug in mega_cmd_done() for megamgr control commands,
 *       the host_byte in the result code from the scsi request to 
 *       scsi midlayer is set to DID_BAD_TARGET when adapter's 
 *       returned codes are 0xF0 and 0xF4.  
 *
 * Version 1.02:
 *     Fixed the tape drive bug by extending the adapter timeout value
 *       for passthrough command to 60 seconds in mega_build_cmd(). 
 *
 * Version 1.03:
 *    Fixed Madrona support.
 *    Changed the adapter timeout value from 60 sec in 1.02 to 10 min
 *      for bigger and slower tape drive.
 *    Added driver version printout at driver loadup time
 *
 * Version 1.04
 *    Added code for 40 ld FW support. 
 *    Added new ioctl command 0x81 to support NEW_READ/WRITE_CONFIG with
 *      data area greater than 4 KB, which is the upper bound for data
 *      tranfer through scsi_ioctl interface.
 *    The addtional 32 bit field for 64bit address in the newly defined
 *      mailbox64 structure is set to 0 at this point.
 *
 * Version 1.05
 *    Changed the queing implementation for handling SCBs and completed
 *      commands.
 *    Added spinlocks in the interrupt service routine to enable the dirver
 *      function in the SMP environment.
 *    Fixed the problem of unnecessary aborts in the abort entry point, which
 *      also enables the driver to handle large amount of I/O requests for
 *      long duration of time.
 *
 * Version 1.07
 *    Removed the usage of uaccess.h file for kernel versions less than
 *    2.0.36, as this file is not present in those versions.
 *
 * Version 1.07b
 *    The MegaRAID 466 cards with 3.00 firmware lockup and seem to very
 *    occasionally hang. We check such cards and report them. You can
 *    get firmware upgrades to flash the board to 3.10 for free.
 *
 * BUGS:
 *     Some older 2.1 kernels (eg. 2.1.90) have a bug in pci.c that
 *     fails to detect the controller as a pci device on the system.
 *
 *     Timeout period for upper scsi layer, i.e. SD_TIMEOUT in
 *     /drivers/scsi/sd.c, is too short for this controller. SD_TIMEOUT
 *     value must be increased to (30 * HZ) otherwise false timeouts 
 *     will occur in the upper layer.
 *
 *===================================================================*/

#define CRLFSTR "\n"
#define IOCTL_CMD_NEW  0x81

#define MEGARAID_VERSION "v107 (December 22, 1999)"


#include <linux/version.h>

#ifdef MODULE
#include <linux/module.h>

char kernel_version[] = UTS_RELEASE;

MODULE_AUTHOR ("American Megatrends Inc.");
MODULE_DESCRIPTION ("AMI MegaRAID driver");
#endif

#include <linux/init.h>
#include <linux/types.h>
#include <linux/errno.h>
#include <linux/kernel.h>
#include <linux/ioport.h>
#include <linux/fcntl.h>
#include <linux/delay.h>
#include <linux/pci.h>
#include <linux/proc_fs.h>
#include <linux/blk.h>
#include <linux/wait.h>
#include <linux/tqueue.h>
#include <linux/interrupt.h>

#include <linux/stat.h>
#include <linux/spinlock.h>

#include <asm/io.h>
#include <asm/irq.h>
#if LINUX_VERSION_CODE > 0x020024
#include <asm/uaccess.h>
#endif

#include "sd.h"
#include "scsi.h"
#include "hosts.h"

#include "megaraid.h"

/*================================================================
 *
 *                          #Defines
 *
 *================================================================
 */

#define MAX_SERBUF 160
#define COM_BASE 0x2f8


u32 RDINDOOR (mega_host_config * megaCfg)
{
  return readl (megaCfg->base + 0x20);
}

void WRINDOOR (mega_host_config * megaCfg, u32 value)
{
  writel (value, megaCfg->base + 0x20);
}

u32 RDOUTDOOR (mega_host_config * megaCfg)
{
  return readl (megaCfg->base + 0x2C);
}

void WROUTDOOR (mega_host_config * megaCfg, u32 value)
{
  writel (value, megaCfg->base + 0x2C);
}

/*================================================================
 *
 *                    Function prototypes
 *
 *================================================================
 */
static int __init megaraid_setup(char *);

static int megaIssueCmd (mega_host_config * megaCfg,
			 u_char * mboxData,
			 mega_scb * scb,
			 int intr);
static int mega_build_sglist (mega_host_config * megaCfg, mega_scb * scb,
			 u32 * buffer, u32 * length);

static int mega_busyWaitMbox(mega_host_config *);
static void mega_runpendq (mega_host_config *);
static void mega_rundoneq (mega_host_config *);
static void mega_cmd_done (mega_host_config *, mega_scb *, int);
static mega_scb *mega_ioctl (mega_host_config * megaCfg, Scsi_Cmnd * SCpnt);
static inline void mega_freeSgList(mega_host_config *megaCfg);
static void mega_Convert8ldTo40ld(  mega_RAIDINQ  *inquiry,
                                    mega_Enquiry3 *enquiry3,
                                    megaRaidProductInfo *productInfo );


#include <linux/smp.h>


#define cpuid smp_processor_id()
#define DRIVER_LOCK_T
#define DRIVER_LOCK_INIT(p)
#define DRIVER_LOCK(p)
#define DRIVER_UNLOCK(p)
#define IO_LOCK_T unsigned long io_flags = 0;
#define IO_LOCK spin_lock_irqsave(&io_request_lock,io_flags);
#define IO_UNLOCK spin_unlock_irqrestore(&io_request_lock,io_flags);

/* set SERDEBUG to 1 to enable serial debugging */
#define SERDEBUG 0
#if SERDEBUG
static void ser_init (void);
static void ser_puts (char *str);
static void ser_putc (char c);
static int ser_printk (const char *fmt,...);
#endif

/*================================================================
 *
 *                    Global variables
 *
 *================================================================
 */

/*  Use "megaraid=skipXX" as LILO option to prohibit driver from scanning
    XX scsi id on each channel.  Used for Madrona motherboard, where SAF_TE
    processor id cannot be scanned */
#ifdef MODULE
static char *megaraid = NULL;
MODULE_PARM(megaraid, "s");
#endif
static int skip_id;

static int numCtlrs = 0;
static mega_host_config *megaCtlrs[FC_MAX_CHANNELS] = {0};

#if DEBUG
static u32 maxCmdTime = 0;
#endif

static mega_scb *pLastScb = NULL;


#if SERDEBUG
static spinlock_t serial_lock = SPIN_LOCK_UNLOCKED;
#endif

#if SERDEBUG
static char strbuf[MAX_SERBUF + 1];

static void ser_init ()
{
  unsigned port = COM_BASE;

  outb (0x80, port + 3);
  outb (0, port + 1);
  /* 9600 Baud, if 19200: outb(6,port) */
  outb (12, port);
  outb (3, port + 3);
  outb (0, port + 1);
}

static void ser_puts (char *str)
{
  char *ptr;

  ser_init ();
  for (ptr = str; *ptr; ++ptr)
    ser_putc (*ptr);
}

static void ser_putc (char c)
{
  unsigned port = COM_BASE;

  while ((inb (port + 5) & 0x20) == 0);
  outb (c, port);
  if (c == 0x0a) {
    while ((inb (port + 5) & 0x20) == 0);
    outb (0x0d, port);
  }
}

static int ser_printk (const char *fmt,...)
{
  va_list args;
  int i;
  long flags;

  spin_lock_irqsave(&serial_lock,flags);
  va_start (args, fmt);
  i = vsprintf (strbuf, fmt, args);
  ser_puts (strbuf);
  va_end (args);
  spin_unlock_irqrestore(&serial_lock,flags);

  return i;
}

#define TRACE(a)    { ser_printk a;}

#else
#define TRACE(A)
#endif

static void callDone (Scsi_Cmnd * SCpnt)
{
  if (SCpnt->result) {
    TRACE (("*** %.08lx %.02x <%d.%d.%d> = %x\n", SCpnt->serial_number,
	    SCpnt->cmnd[0], SCpnt->channel, SCpnt->target, SCpnt->lun,
	    SCpnt->result));
  }
  SCpnt->scsi_done (SCpnt);
}

/*-------------------------------------------------------------------------
 *
 *                      Local functions
 *
 *-------------------------------------------------------------------------*/

/*=======================
 * Free a SCB structure
 *=======================
 */
static void mega_freeSCB (mega_host_config *megaCfg, mega_scb * pScb)
{

  mega_scb *pScbtmp;

  if ((pScb == NULL) || (pScb->idx >= 0xFE)) {
    return ;
  }

  /* Unlink from pending queue */

  if(pScb == megaCfg->qPendingH) {
    if(megaCfg->qPendingH == megaCfg->qPendingT ) 
      megaCfg->qPendingH = megaCfg->qPendingT = NULL;
    else {
      megaCfg->qPendingH = megaCfg->qPendingH->next;
    }
    megaCfg->qPcnt--;
  }
  else {
    for(pScbtmp=megaCfg->qPendingH; pScbtmp; pScbtmp=pScbtmp->next) {
      if (pScbtmp->next == pScb) {
        pScbtmp->next = pScb->next;
        if(pScb == megaCfg->qPendingT) {
          megaCfg->qPendingT = pScbtmp;
        }
        megaCfg->qPcnt--;
	break;
    }
  }
  }

  /* Link back into free list */
  pScb->state = SCB_FREE;
  pScb->SCpnt = NULL;

  if(megaCfg->qFreeH == (mega_scb *) NULL ) {
    megaCfg->qFreeH = megaCfg->qFreeT = pScb;
  }
  else {
    megaCfg->qFreeT->next  = pScb;
    megaCfg->qFreeT = pScb;
  }
  megaCfg->qFreeT->next = NULL;
  megaCfg->qFcnt++;

}

/*===========================
 * Allocate a SCB structure
 *===========================
 */
static mega_scb * mega_allocateSCB (mega_host_config * megaCfg, Scsi_Cmnd * SCpnt)
{
  mega_scb *pScb;

  /* Unlink command from Free List */
  if ((pScb = megaCfg->qFreeH) != NULL) {
    megaCfg->qFreeH = pScb->next;
    megaCfg->qFcnt--;
    
    pScb->isrcount = jiffies;
    pScb->next  = NULL;
    pScb->state = SCB_ACTIVE;
    pScb->SCpnt = SCpnt;

    return pScb;
  }

  printk (KERN_WARNING "Megaraid: Could not allocate free SCB!!!\n");

  return NULL;
}

/*================================================
 * Initialize SCB structures
 *================================================
 */
static int mega_initSCB (mega_host_config * megaCfg)
{
  int idx;

  megaCfg->qFreeH = NULL;
  megaCfg->qFcnt = 0;
#if DEBUG
if(megaCfg->max_cmds >= MAX_COMMANDS) {
printk("megaraid:ctlr max cmds = %x : MAX_CMDS = %x", megaCfg->max_cmds, MAX_COMMANDS);
}
#endif

  for (idx = megaCfg->max_cmds-1; idx >= 0; idx--) {
    megaCfg->scbList[idx].idx    = idx;
    megaCfg->scbList[idx].sgList = kmalloc(sizeof(mega_sglist) * MAX_SGLIST,
					   GFP_ATOMIC | GFP_DMA);
    if (megaCfg->scbList[idx].sgList == NULL) {
      printk(KERN_WARNING "Can't allocate sglist for id %d\n",idx);
      mega_freeSgList(megaCfg);
      return -1;
    }
    
    if (idx < MAX_COMMANDS) {
      /* Link to free list */
      mega_freeSCB(megaCfg, &megaCfg->scbList[idx]);
    }
  }
  return 0;
}

/* Run through the list of completed requests */
static void mega_rundoneq (mega_host_config *megaCfg)
{
  Scsi_Cmnd *SCpnt;

  while ((SCpnt = megaCfg->qCompletedH) != NULL) {
    megaCfg->qCompletedH = (Scsi_Cmnd *)SCpnt->host_scribble;
    megaCfg->qCcnt--;

    SCpnt->host_scribble = (unsigned char *) NULL ; // XC : sep 14
    /* Callback */
    callDone (SCpnt);
  }
  megaCfg->qCompletedH = megaCfg->qCompletedT = NULL;
}

/*
 * Runs through the list of pending requests
 * Assumes that mega_lock spin_lock has been acquired.
 */
static void mega_runpendq(mega_host_config *megaCfg)
{
  mega_scb *pScb;

  /* Issue any pending commands to the card */
  for(pScb=megaCfg->qPendingH; pScb; pScb=pScb->next) {
    if (pScb->state == SCB_ACTIVE) {
      if(megaIssueCmd(megaCfg, pScb->mboxData, pScb, 1))
	return;
    }
  }
}

/* Add command to the list of completed requests */
static void
mega_cmd_done (mega_host_config * megaCfg, mega_scb * pScb, 
			   int status)
{
  int islogical;
  Scsi_Cmnd *SCpnt;
  mega_passthru *pthru;
  mega_mailbox *mbox;

  if (pScb == NULL) {
	TRACE(("NULL pScb in mega_cmd_done!"));
	printk("NULL pScb in mega_cmd_done!");
  }

  SCpnt = pScb->SCpnt;
  pthru = &pScb->pthru;
  mbox = (mega_mailbox *) &pScb->mboxData;

  if (SCpnt == NULL) {
	TRACE(("NULL SCpnt in mega_cmd_done!"));
	TRACE(("pScb->idx = ",pScb->idx));
	TRACE(("pScb->state = ",pScb->state));
	TRACE(("pScb->state = ",pScb->state));
	printk("megaraid:Problem...!\n");
	while(1);
  }

  islogical = (SCpnt->channel == megaCfg->host->max_channel);

  if (SCpnt->cmnd[0] == INQUIRY &&
      ((((u_char *) SCpnt->request_buffer)[0] & 0x1F) == TYPE_DISK) &&
      !islogical) {
    status = 0xF0;
  }

/* clear result; otherwise, success returns corrupt value */ 
 SCpnt->result = 0;  

if ((SCpnt->cmnd[0] & 0x80) ) {/* i.e. ioctl cmd such as 0x80, 0x81 of megamgr*/
    switch (status) {
      case 0xF0:
      case 0xF4:
	SCpnt->result=(DID_BAD_TARGET<<16)|status;
        break;
      default:
	SCpnt->result|=status;
   }/*end of switch*/
}
else{
  /* Convert MegaRAID status to Linux error code */
  switch (status) {
  case 0x00: /* SUCCESS , i.e. SCSI_STATUS_GOOD*/
    SCpnt->result |= (DID_OK << 16);
    break;
  case 0x02: /* ERROR_ABORTED, i.e. SCSI_STATUS_CHECK_CONDITION */
	/*set sense_buffer and result fields*/
       if( mbox->cmd==MEGA_MBOXCMD_PASSTHRU ){
	  memcpy( SCpnt->sense_buffer , pthru->reqsensearea, 14);
          SCpnt->result = (DRIVER_SENSE<<24)|(DID_ERROR << 16)|status; 
       }
       else{
 	  SCpnt->sense_buffer[0]=0x70;
	  SCpnt->sense_buffer[2]=ABORTED_COMMAND;
          SCpnt->result |= (CHECK_CONDITION << 1);
       }
    break;
  case 0x08:  /* ERR_DEST_DRIVE_FAILED, i.e. SCSI_STATUS_BUSY */
    SCpnt->result |= (DID_BUS_BUSY << 16)|status;
    break;
  default: 
    SCpnt->result |= (DID_BAD_TARGET << 16)|status;
    break;
  }
 }
  if ( SCpnt->cmnd[0]!=IOCTL_CMD_NEW ) 
  /* not IOCTL_CMD_NEW SCB, freeSCB()*/
  /* For IOCTL_CMD_NEW SCB, delay freeSCB() in megaraid_queue()
   * after copy data back to user space*/
     mega_freeSCB(megaCfg, pScb);

  /* Add Scsi_Command to end of completed queue */
    if( megaCfg->qCompletedH == NULL ) {
      megaCfg->qCompletedH = megaCfg->qCompletedT = SCpnt;
    }
    else {
      megaCfg->qCompletedT->host_scribble = (unsigned char *) SCpnt;
      megaCfg->qCompletedT = SCpnt;
   }
   megaCfg->qCompletedT->host_scribble = (unsigned char *) NULL;
   megaCfg->qCcnt++;
}

/*-------------------------------------------------------------------
 *
 *                 Build a SCB from a Scsi_Cmnd
 *
 * Returns a SCB pointer, or NULL
 * If NULL is returned, the scsi_done function MUST have been called
 *
 *-------------------------------------------------------------------*/
static mega_scb * mega_build_cmd (mega_host_config * megaCfg, 
				  Scsi_Cmnd * SCpnt)
{
  mega_scb *pScb;
  mega_mailbox *mbox;
  mega_passthru *pthru;
  long seg;
  char islogical;
  char lun = SCpnt->lun;

  if ((SCpnt->cmnd[0] == 0x80)  || (SCpnt->cmnd[0] == IOCTL_CMD_NEW) )  /* ioctl */
    return mega_ioctl (megaCfg, SCpnt);
 
  islogical = (SCpnt->channel == megaCfg->host->max_channel);

  if (!islogical && lun != 0) {
    SCpnt->result = (DID_BAD_TARGET << 16);
    callDone (SCpnt);
    return NULL;
  }

  if (!islogical && SCpnt->target == skip_id) {
	SCpnt->result = (DID_BAD_TARGET << 16);
	callDone (SCpnt);
	return NULL;
  }

  if ( islogical ) {
	lun = (SCpnt->target * 8) + lun;
        if ( lun > FC_MAX_LOGICAL_DRIVES ){
            SCpnt->result = (DID_BAD_TARGET << 16);
            callDone (SCpnt);
            return NULL;
        }
  }
  /*-----------------------------------------------------
   *
   *               Logical drive commands
   *
   *-----------------------------------------------------*/
  if (islogical) {
    switch (SCpnt->cmnd[0]) {
    case TEST_UNIT_READY:
      memset (SCpnt->request_buffer, 0, SCpnt->request_bufflen);
      SCpnt->result = (DID_OK << 16);
      callDone (SCpnt);
      return NULL;

    case MODE_SENSE:
      memset (SCpnt->request_buffer, 0, SCpnt->cmnd[4]);
      SCpnt->result = (DID_OK << 16);
      callDone (SCpnt);
      return NULL;

    case READ_CAPACITY:
    case INQUIRY:
      /* Allocate a SCB and initialize passthru */
      if ((pScb = mega_allocateSCB (megaCfg, SCpnt)) == NULL) {
	SCpnt->result = (DID_ERROR << 16);
	callDone (SCpnt);
	return NULL;
      }
      pthru = &pScb->pthru;
      mbox = (mega_mailbox *) & pScb->mboxData;

      memset (mbox, 0, sizeof (pScb->mboxData));
      memset (pthru, 0, sizeof (mega_passthru));
      pthru->timeout = 0;
      pthru->ars = 1;
      pthru->reqsenselen = 14;
      pthru->islogical = 1;
      pthru->logdrv = lun;
      pthru->cdblen = SCpnt->cmd_len;
      pthru->dataxferaddr = virt_to_bus (SCpnt->request_buffer);
      pthru->dataxferlen = SCpnt->request_bufflen;
      memcpy (pthru->cdb, SCpnt->cmnd, SCpnt->cmd_len);

      /* Initialize mailbox area */
      mbox->cmd = MEGA_MBOXCMD_PASSTHRU;
      mbox->xferaddr = virt_to_bus (pthru);

      return pScb;

    case READ_6:
    case WRITE_6:
    case READ_10:
    case WRITE_10:
      /* Allocate a SCB and initialize mailbox */
      if ((pScb = mega_allocateSCB (megaCfg, SCpnt)) == NULL) {
	SCpnt->result = (DID_ERROR << 16);
	callDone (SCpnt);
	return NULL;
      }
      mbox = (mega_mailbox *) & pScb->mboxData;

      memset (mbox, 0, sizeof (pScb->mboxData));
      mbox->logdrv = lun;
      mbox->cmd = (*SCpnt->cmnd == READ_6 || *SCpnt->cmnd == READ_10) ?
	MEGA_MBOXCMD_LREAD : MEGA_MBOXCMD_LWRITE;

      /* 6-byte */
      if (*SCpnt->cmnd == READ_6 || *SCpnt->cmnd == WRITE_6) {
	mbox->numsectors =
	  (u32) SCpnt->cmnd[4];
	mbox->lba =
	  ((u32) SCpnt->cmnd[1] << 16) |
	  ((u32) SCpnt->cmnd[2] << 8) |
	  (u32) SCpnt->cmnd[3];
	mbox->lba &= 0x1FFFFF;
      }

      /* 10-byte */
      if (*SCpnt->cmnd == READ_10 || *SCpnt->cmnd == WRITE_10) {
	mbox->numsectors =
	  (u32) SCpnt->cmnd[8] |
	  ((u32) SCpnt->cmnd[7] << 8);
	mbox->lba =
	  ((u32) SCpnt->cmnd[2] << 24) |
	  ((u32) SCpnt->cmnd[3] << 16) |
	  ((u32) SCpnt->cmnd[4] << 8) |
	  (u32) SCpnt->cmnd[5];
      }

      /* Calculate Scatter-Gather info */
      mbox->numsgelements = mega_build_sglist (megaCfg, pScb,
					  (u32 *) & mbox->xferaddr,
					  (u32 *) & seg);

      return pScb;

    default:
      SCpnt->result = (DID_BAD_TARGET << 16);
      callDone (SCpnt);
      return NULL;
    }
  }
  /*-----------------------------------------------------
   *
   *               Passthru drive commands
   *
   *-----------------------------------------------------*/
  else {
    /* Allocate a SCB and initialize passthru */
    if ((pScb = mega_allocateSCB (megaCfg, SCpnt)) == NULL) {
      SCpnt->result = (DID_ERROR << 16);
      callDone (SCpnt);
      return NULL;
    }
    pthru = &pScb->pthru;
    mbox = (mega_mailbox *) pScb->mboxData;

    memset (mbox, 0, sizeof (pScb->mboxData));
    memset (pthru, 0, sizeof (mega_passthru));
    pthru->timeout = 2; /*set adapter timeout value to 10 min. for tape drive*/
     		        /* 0=6sec/1=60sec/2=10min/3=3hrs */
    pthru->ars = 1;
    pthru->reqsenselen = 14;
    pthru->islogical = 0;
    pthru->channel = (megaCfg->flag & BOARD_40LD) ? 0 : SCpnt->channel;
    pthru->target = (megaCfg->flag & BOARD_40LD) ? /*BOARD_40LD*/
                     (SCpnt->channel<<4)|SCpnt->target : SCpnt->target;
    pthru->cdblen = SCpnt->cmd_len;
    memcpy (pthru->cdb, SCpnt->cmnd, SCpnt->cmd_len);

    pthru->numsgelements = mega_build_sglist (megaCfg, pScb,
					 (u32 *) & pthru->dataxferaddr,
					 (u32 *) & pthru->dataxferlen);

    /* Initialize mailbox */
    mbox->cmd = MEGA_MBOXCMD_PASSTHRU;
    mbox->xferaddr = virt_to_bus (pthru);

    return pScb;
  }
  return NULL;
}

/*--------------------------------------------------------------------
 * build RAID commands for controller, passed down through ioctl()
 *--------------------------------------------------------------------*/
static mega_scb * mega_ioctl (mega_host_config * megaCfg, Scsi_Cmnd * SCpnt)
{
  mega_scb *pScb;
  mega_ioctl_mbox *mbox;
  mega_mailbox *mailbox;
  mega_passthru *pthru;
  u8 *mboxdata;
  long seg;
  unsigned char *data = (unsigned char *)SCpnt->request_buffer;
  int i;

  if ((pScb = mega_allocateSCB (megaCfg, SCpnt)) == NULL) {
    SCpnt->result = (DID_ERROR << 16);
    callDone (SCpnt);
    return NULL;
  }

  mboxdata = (u8 *) & pScb->mboxData;
  mbox = (mega_ioctl_mbox *) & pScb->mboxData;
  mailbox = (mega_mailbox *) & pScb->mboxData;
  memset (mailbox, 0, sizeof (pScb->mboxData));

  if (data[0] == 0x03) {	/* passthrough command */
    unsigned char cdblen = data[2];
    pthru = &pScb->pthru;
    memset (pthru, 0, sizeof (mega_passthru));
    pthru->islogical = (data[cdblen+3] & 0x80) ? 1:0;
    pthru->timeout = data[cdblen+3] & 0x07;
    pthru->reqsenselen = 14;
    pthru->ars = (data[cdblen+3] & 0x08) ? 1:0;
    pthru->logdrv = data[cdblen+4];
    pthru->channel = data[cdblen+5];
    pthru->target = data[cdblen+6];
    pthru->cdblen = cdblen;
    memcpy (pthru->cdb, &data[3], cdblen);

    mailbox->cmd = MEGA_MBOXCMD_PASSTHRU;
    mailbox->xferaddr = virt_to_bus (pthru);

    pthru->numsgelements = mega_build_sglist (megaCfg, pScb,
					 (u32 *) & pthru->dataxferaddr,
					 (u32 *) & pthru->dataxferlen);

    for (i=0;i<(SCpnt->request_bufflen-cdblen-7);i++) {
       data[i] = data[i+cdblen+7];
    }

    return pScb;
  }
  /* else normal (nonpassthru) command */

#if LINUX_VERSION_CODE > 0x020024
/*
 * usage of the function copy from user is used in case of data more than
 * 4KB.  This is used only with adapters which supports more than 8 logical
 * drives.  This feature is disabled on kernels earlier or same as 2.0.36
 * as the uaccess.h file is not available with those kernels.
 */

  if (SCpnt->cmnd[0] == IOCTL_CMD_NEW) { 
            /* use external data area for large xfers  */
     /* If cmnd[0] is set to IOCTL_CMD_NEW then *
      *   cmnd[4..7] = external user buffer     *
      *   cmnd[8..11] = length of buffer        *
      *                                         */
      char *kern_area;
      char *user_area = *((char **)&SCpnt->cmnd[4]);
      u32 xfer_size = *((u32 *)&SCpnt->cmnd[8]);
      if (verify_area(VERIFY_READ, user_area, xfer_size)) {
          printk("megaraid: Got bad user address.\n");
          SCpnt->result = (DID_ERROR << 16);
          callDone (SCpnt);
          return NULL;
      }
      kern_area = kmalloc(xfer_size, GFP_ATOMIC | GFP_DMA);
      if (kern_area == NULL) {
          printk("megaraid: Couldn't allocate kernel mem.\n");
	  SCpnt->result = (DID_ERROR << 16);
	  callDone (SCpnt);
	  return NULL;
      }
      copy_from_user(kern_area,user_area,xfer_size);
      pScb->kern_area = kern_area;
  }
#endif

  mbox->cmd = data[0];
  mbox->channel = data[1];
  mbox->param = data[2];
  mbox->pad[0] = data[3];
  mbox->logdrv = data[4];

  if(SCpnt->cmnd[0] == IOCTL_CMD_NEW) {
      if(data[0]==DCMD_FC_CMD){ /*i.e. 0xA1, then override some mbox data */
          *(mboxdata+0) = data[0]; /*mailbox byte 0: DCMD_FC_CMD*/
          *(mboxdata+2) = data[2]; /*sub command*/
          *(mboxdata+3) = 0;       /*number of elements in SG list*/
          mbox->xferaddr           /*i.e. mboxdata byte 0x8 to 0xb*/
                        = virt_to_bus(pScb->kern_area);
      }
      else{
         mbox->xferaddr = virt_to_bus(pScb->kern_area);
         mbox->numsgelements = 0;
      }
  } 
  else {

      mbox->numsgelements = mega_build_sglist (megaCfg, pScb,
				      (u32 *) & mbox->xferaddr,
				      (u32 *) & seg);

      for (i=0;i<(SCpnt->request_bufflen-6);i++) {
          data[i] = data[i+6];
      }
  }

  return (pScb);
}

#if DEBUG
static void showMbox(mega_scb *pScb)
{
  mega_mailbox *mbox;

  if (pScb == NULL) return;

  mbox = (mega_mailbox *)pScb->mboxData;
  printk("%u cmd:%x id:%x #scts:%x lba:%x addr:%x logdrv:%x #sg:%x\n",
	 pScb->SCpnt->pid, 
	 mbox->cmd, mbox->cmdid, mbox->numsectors,
	 mbox->lba, mbox->xferaddr, mbox->logdrv,
	 mbox->numsgelements);
}
#endif

#if DEBUG
static unsigned int cum_time = 0;
static unsigned int cum_time_cnt = 0;
#endif

/*--------------------------------------------------------------------
 * Interrupt service routine
 *--------------------------------------------------------------------*/
static void megaraid_isr (int irq, void *devp, struct pt_regs *regs)
{
#if LINUX_VERSION_CODE >= 0x20100
  IO_LOCK_T
#endif
  mega_host_config    *megaCfg;
  u_char byte, idx, sIdx, tmpBox[MAILBOX_SIZE];
  u32 dword=0;
  mega_mailbox *mbox;
  mega_scb *pScb;
  u_char qCnt, qStatus;
  u_char            completed[MAX_FIRMWARE_STATUS];
  Scsi_Cmnd *SCpnt;

  megaCfg = (mega_host_config *) devp;
  mbox = (mega_mailbox *)tmpBox;

  if (megaCfg->host->irq == irq) {
    if (megaCfg->flag & IN_ISR) {
      printk(KERN_ERR "ISR called reentrantly!!\n");
    }

    megaCfg->flag |= IN_ISR;

    if (mega_busyWaitMbox(megaCfg)) {
	printk(KERN_WARNING "Error: mailbox busy in isr!\n");
    }

    /* Check if a valid interrupt is pending */
    if (megaCfg->flag & BOARD_QUARTZ) {
      dword = RDOUTDOOR (megaCfg);
      if (dword != 0x10001234) {
	/* Spurious interrupt */
	megaCfg->flag &= ~IN_ISR;
	return;
      }
    }
    else {
      byte = READ_PORT (megaCfg->host->io_port, INTR_PORT);
      if ((byte & VALID_INTR_BYTE) == 0) {
	/* Spurious interrupt */
	megaCfg->flag &= ~IN_ISR;
	return;
      }
      WRITE_PORT (megaCfg->host->io_port, INTR_PORT, byte);
    }

    for(idx=0;idx<MAX_FIRMWARE_STATUS;idx++ ) completed[idx] = 0;

    IO_LOCK;

    qCnt = 0xff;
    while ((qCnt = megaCfg->mbox->numstatus) == 0xFF) 
      ;

    qStatus = 0xff;
    while ((qStatus = megaCfg->mbox->status) == 0xFF)
      ;

    /* Get list of completed requests */
    for (idx = 0; idx<qCnt; idx++) {
      while ((sIdx = megaCfg->mbox->completed[idx]) == 0xFF) {
	printk("p");
      }
      completed[idx] = sIdx;
      sIdx = 0xFF;
    }

    if (megaCfg->flag & BOARD_QUARTZ) {
      WROUTDOOR (megaCfg, dword);
      /* Acknowledge interrupt */
      WRINDOOR (megaCfg, virt_to_bus (megaCfg->mbox) | 0x2);
      while (RDINDOOR (megaCfg) & 0x02);
    }
    else {
      CLEAR_INTR (megaCfg->host->io_port);
    }

#if DEBUG
    if(qCnt >= MAX_FIRMWARE_STATUS) {
      printk("megaraid_isr: cmplt=%d ", qCnt);
    }
#endif

    for (idx = 0; idx < qCnt; idx++) {
      sIdx = completed[idx];
      if ((sIdx > 0) && (sIdx <= MAX_COMMANDS)) {
	pScb = &megaCfg->scbList[sIdx - 1];

	/* ASSERT(pScb->state == SCB_ISSUED); */

#if DEBUG
	if (((jiffies) - pScb->isrcount) > maxCmdTime) {
	  maxCmdTime = (jiffies) - pScb->isrcount;
	  printk("megaraid_isr : cmd time = %u\n", maxCmdTime);
	}
#endif
/*
 * Assuming that the scsi command, for which an abort request was received
 * earlier has completed.
 */
	if (pScb->state == SCB_ABORTED) {
          SCpnt = pScb->SCpnt;
	}
	if (pScb->state == SCB_RESET) {
          SCpnt = pScb->SCpnt;
	  mega_freeSCB (megaCfg, pScb);
	  SCpnt->result = (DID_RESET << 16) ;
          if( megaCfg->qCompletedH == NULL ) {
            megaCfg->qCompletedH = megaCfg->qCompletedT = SCpnt;
          }
          else {
            megaCfg->qCompletedT->host_scribble = (unsigned char *) SCpnt;
            megaCfg->qCompletedT = SCpnt;
          }
          megaCfg->qCompletedT->host_scribble = (unsigned char *) NULL;
          megaCfg->qCcnt++;
          continue;
	}

        if (*(pScb->SCpnt->cmnd)==IOCTL_CMD_NEW) 
        {    /* external user buffer */
           up(&pScb->sem);
        }
	/* Mark command as completed */
	mega_cmd_done(megaCfg, pScb, qStatus);

      }
      else {
        printk(KERN_ERR "megaraid: wrong cmd id completed from firmware:id=%x\n",sIdx);
      }
    }

    mega_rundoneq(megaCfg);

    megaCfg->flag &= ~IN_ISR;

    /* Loop through any pending requests */
    mega_runpendq(megaCfg);
#if LINUX_VERSION_CODE >= 0x20100
    IO_UNLOCK;
#endif

  }
}

/*==================================================*/
/* Wait until the controller's mailbox is available */
/*==================================================*/
static int mega_busyWaitMbox (mega_host_config * megaCfg)
{
  mega_mailbox *mbox = (mega_mailbox *) megaCfg->mbox;
  long counter;

  for (counter = 0; counter < 10000; counter++) {
    if (!mbox->busy) {
      return 0;
    }
    udelay (100);
    barrier();
  }
  return -1;			/* give up after 1 second */
}

/*=====================================================
 * Post a command to the card
 *
 * Arguments:
 *   mega_host_config *megaCfg - Controller structure
 *   u_char *mboxData - Mailbox area, 16 bytes
 *   mega_scb *pScb   - SCB posting (or NULL if N/A)
 *   int intr         - if 1, interrupt, 0 is blocking
 * Return Value: (added on 7/26 for 40ld/64bit)
 *   -1: the command was not actually issued out
 *   othercases:
 *     intr==0, return ScsiStatus, i.e. mbox->status
 *     intr==1, return 0 
 *=====================================================
 */
static int megaIssueCmd (mega_host_config * megaCfg,
	      u_char * mboxData,
	      mega_scb * pScb,
	      int intr)
{
  mega_mailbox *mbox = (mega_mailbox *) megaCfg->mbox;
  u_char byte;
  u32 cmdDone;
  u32 phys_mbox;
  u8 retval=-1;

  mboxData[0x1] = (pScb ? pScb->idx + 1: 0x0);   /* Set cmdid */
  mboxData[0xF] = 1;		/* Set busy */

  phys_mbox = virt_to_bus (megaCfg->mbox);

#if DEBUG
  showMbox(pScb);
#endif

  /* Wait until mailbox is free */
  if (mega_busyWaitMbox (megaCfg)) {
    printk("Blocked mailbox......!!\n");
    udelay(1000);

#if DEBUG
    showMbox(pLastScb);
#endif
    
    /* Abort command */
    if (pScb == NULL) {
	TRACE(("NULL pScb in megaIssue\n"));
	printk("NULL pScb in megaIssue\n");
    }
    mega_cmd_done (megaCfg, pScb, 0x08);
    return -1;
  }

  pLastScb = pScb;

  /* Copy mailbox data into host structure */
  megaCfg->mbox64->xferSegment = 0;
  memcpy (mbox, mboxData, 16);

  /* Kick IO */
  if (intr) {

    /* Issue interrupt (non-blocking) command */
    if (megaCfg->flag & BOARD_QUARTZ) {
       mbox->mraid_poll = 0;
      mbox->mraid_ack = 0;
      WRINDOOR (megaCfg, phys_mbox | 0x1);
    }
    else {
      ENABLE_INTR (megaCfg->host->io_port);
      ISSUE_COMMAND (megaCfg->host->io_port);
    }
    pScb->state = SCB_ISSUED;

    retval=0;
  }
  else {			/* Issue non-ISR (blocking) command */
    disable_irq(megaCfg->host->irq);
    if (megaCfg->flag & BOARD_QUARTZ) {
      mbox->mraid_poll = 0;
      mbox->mraid_ack = 0;
      WRINDOOR (megaCfg, phys_mbox | 0x1);

      while ((cmdDone = RDOUTDOOR (megaCfg)) != 0x10001234);
      WROUTDOOR (megaCfg, cmdDone);

      if (pScb) {
	mega_cmd_done (megaCfg, pScb, mbox->status);
      }

      WRINDOOR (megaCfg, phys_mbox | 0x2);
      while (RDINDOOR (megaCfg) & 0x2);

    }
    else {
      DISABLE_INTR (megaCfg->host->io_port);
      ISSUE_COMMAND (megaCfg->host->io_port);

      while (!((byte = READ_PORT (megaCfg->host->io_port, INTR_PORT)) & INTR_VALID));
      WRITE_PORT (megaCfg->host->io_port, INTR_PORT, byte);

      ENABLE_INTR (megaCfg->host->io_port);
      CLEAR_INTR (megaCfg->host->io_port);

      if (pScb) {
	mega_cmd_done (megaCfg, pScb, mbox->status);
      }
      else {
	TRACE (("Error: NULL pScb!\n"));
      }
    }
    enable_irq(megaCfg->host->irq);
    retval=mbox->status;
  }
#if DEBUG
  while (mega_busyWaitMbox (megaCfg)) {
    printk("Blocked mailbox on exit......!\n");
    udelay(1000);
  }
#endif

  return retval;
}

/*-------------------------------------------------------------------
 * Copies data to SGLIST
 *-------------------------------------------------------------------*/
static int mega_build_sglist (mega_host_config * megaCfg, mega_scb * scb,
	      u32 * buffer, u32 * length)
{
  struct scatterlist *sgList;
  int idx;

  /* Scatter-gather not used */
  if (scb->SCpnt->use_sg == 0) {
    *buffer = virt_to_bus (scb->SCpnt->request_buffer);
    *length = (u32) scb->SCpnt->request_bufflen;
    return 0;
  }

  sgList = (struct scatterlist *) scb->SCpnt->request_buffer;
  if (scb->SCpnt->use_sg == 1) {
    *buffer = virt_to_bus (sgList[0].address);
    *length = (u32) sgList[0].length;
    return 0;
  }

  /* Copy Scatter-Gather list info into controller structure */
  for (idx = 0; idx < scb->SCpnt->use_sg; idx++) {
    scb->sgList[idx].address = virt_to_bus (sgList[idx].address);
    scb->sgList[idx].length = (u32) sgList[idx].length;
  }

  /* Reset pointer and length fields */
  *buffer = virt_to_bus (scb->sgList);
  *length = 0;

  /* Return count of SG requests */
  return scb->SCpnt->use_sg;
}

/*--------------------------------------------------------------------
 * Initializes the adress of the controller's mailbox register
 *  The mailbox register is used to issue commands to the card.
 *  Format of the mailbox area:
 *   00 01 command
 *   01 01 command id
 *   02 02 # of sectors
 *   04 04 logical bus address
 *   08 04 physical buffer address
 *   0C 01 logical drive #
 *   0D 01 length of scatter/gather list
 *   0E 01 reserved
 *   0F 01 mailbox busy
 *   10 01 numstatus byte
 *   11 01 status byte
 *--------------------------------------------------------------------*/
static int mega_register_mailbox (mega_host_config * megaCfg, u32 paddr)
{
  /* align on 16-byte boundry */
  megaCfg->mbox = &megaCfg->mailbox64.mailbox;
  megaCfg->mbox = (mega_mailbox *) ((((u32) megaCfg->mbox) + 16) & 0xfffffff0);
  megaCfg->mbox64 = (mega_mailbox64 *) (megaCfg->mbox - 4);
  paddr = (paddr + 4 + 16) & 0xfffffff0;

  /* Register mailbox area with the firmware */
  if (!(megaCfg->flag & BOARD_QUARTZ)) {
    WRITE_PORT (megaCfg->host->io_port, MBOX_PORT0, paddr & 0xFF);
    WRITE_PORT (megaCfg->host->io_port, MBOX_PORT1, (paddr >> 8) & 0xFF);
    WRITE_PORT (megaCfg->host->io_port, MBOX_PORT2, (paddr >> 16) & 0xFF);
    WRITE_PORT (megaCfg->host->io_port, MBOX_PORT3, (paddr >> 24) & 0xFF);
    WRITE_PORT (megaCfg->host->io_port, ENABLE_MBOX_REGION, ENABLE_MBOX_BYTE);

    CLEAR_INTR (megaCfg->host->io_port);
    ENABLE_INTR (megaCfg->host->io_port);
  }
  return 0;
}


/*---------------------------------------------------------------------------
 * mega_Convert8ldTo40ld() -- takes all info in AdapterInquiry structure and
 * puts it into ProductInfo and Enquiry3 structures for later use
 *---------------------------------------------------------------------------*/
static void mega_Convert8ldTo40ld(  mega_RAIDINQ  *inquiry,
                                    mega_Enquiry3 *enquiry3,
                                    megaRaidProductInfo *productInfo )
{
        int i;

        productInfo->MaxConcCmds = inquiry->AdpInfo.MaxConcCmds;
        enquiry3->rbldRate = inquiry->AdpInfo.RbldRate;
        productInfo->SCSIChanPresent = inquiry->AdpInfo.ChanPresent;
        for (i=0;i<4;i++) {
                productInfo->FwVer[i] = inquiry->AdpInfo.FwVer[i];
                productInfo->BiosVer[i] = inquiry->AdpInfo.BiosVer[i];
        }
        enquiry3->cacheFlushInterval = inquiry->AdpInfo.CacheFlushInterval;
        productInfo->DramSize = inquiry->AdpInfo.DramSize;

        enquiry3->numLDrv = inquiry->LogdrvInfo.NumLDrv;
        for (i=0;i<MAX_LOGICAL_DRIVES;i++) {
                enquiry3->lDrvSize[i] = inquiry->LogdrvInfo.LDrvSize[i];
                enquiry3->lDrvProp[i] = inquiry->LogdrvInfo.LDrvProp[i];
                enquiry3->lDrvState[i] = inquiry->LogdrvInfo.LDrvState[i];
        }

        for (i=0;i<(MAX_PHYSICAL_DRIVES);i++) {
                enquiry3->pDrvState[i] = inquiry->PhysdrvInfo.PDrvState[i];
        }
}


/*-------------------------------------------------------------------
 * Issue an adapter info query to the controller
 *-------------------------------------------------------------------*/
static int mega_i_query_adapter (mega_host_config * megaCfg)
{
  mega_Enquiry3 *enquiry3Pnt;
  mega_mailbox *mbox;
  u_char mboxData[16];
  u32 paddr;
  u8 retval;

  /* Initialize adapter inquiry mailbox*/
  paddr = virt_to_bus (megaCfg->mega_buffer);
  mbox = (mega_mailbox *) mboxData;

  memset ((void *) megaCfg->mega_buffer, 0, sizeof (megaCfg->mega_buffer));
  memset (mbox, 0, 16);

/*
 * Try to issue Enquiry3 command 
 * if not suceeded, then issue MEGA_MBOXCMD_ADAPTERINQ command and 
 * update enquiry3 structure
 */
  mbox->xferaddr = virt_to_bus ( (void*) megaCfg->mega_buffer); 
             /* Initialize mailbox databuffer addr */
  enquiry3Pnt = (mega_Enquiry3 *) megaCfg->mega_buffer; 
             /* point mega_Enguiry3 to the data buf */

  mboxData[0]=FC_NEW_CONFIG ;          /* i.e. mbox->cmd=0xA1 */
  mboxData[2]=NC_SUBOP_ENQUIRY3;       /* i.e. 0x0F */
  mboxData[3]=ENQ3_GET_SOLICITED_FULL; /* i.e. 0x02 */

  /* Issue a blocking command to the card */
  if ( (retval=megaIssueCmd(megaCfg, mboxData, NULL, 0)) != 0 )
  {  /* the adapter does not support 40ld*/

     mega_RAIDINQ adapterInquiryData;
     mega_RAIDINQ *adapterInquiryPnt = &adapterInquiryData;

     mbox->xferaddr = virt_to_bus ( (void*) adapterInquiryPnt);

     mbox->cmd = MEGA_MBOXCMD_ADAPTERINQ;  /*issue old 0x05 command to adapter*/
     /* Issue a blocking command to the card */;
     retval=megaIssueCmd (megaCfg, mboxData, NULL, 0);

     /*update Enquiry3 and ProductInfo structures with mega_RAIDINQ structure*/
     mega_Convert8ldTo40ld(  adapterInquiryPnt, 
                             enquiry3Pnt, 
                             (megaRaidProductInfo * ) &megaCfg->productInfo );

  }
  else{ /* adapter supports 40ld */
    megaCfg->flag |= BOARD_40LD;

    /*get productInfo, which is static information and will be unchanged*/
    mbox->xferaddr = virt_to_bus ( (void*) &megaCfg->productInfo );

    mboxData[0]=FC_NEW_CONFIG ;         /* i.e. mbox->cmd=0xA1 */
    mboxData[2]=NC_SUBOP_PRODUCT_INFO;  /* i.e. 0x0E */
   
    if( (retval=megaIssueCmd(megaCfg, mboxData, NULL, 0)) != 0  ) 
        printk("ami:Product_info (0x0E) cmd failed with error: %d\n", retval);

  }

  megaCfg->host->max_channel = megaCfg->productInfo.SCSIChanPresent;
  megaCfg->host->max_id = 16;              /* max targets per channel */
    /*(megaCfg->flag & BOARD_40LD)?FC_MAX_TARGETS_PER_CHANNEL:MAX_TARGET+1;*/ 
  megaCfg->host->max_lun =              /* max lun */
    (megaCfg->flag & BOARD_40LD) ? FC_MAX_LOGICAL_DRIVES : MAX_LOGICAL_DRIVES; 
  megaCfg->host->cmd_per_lun = MAX_CMD_PER_LUN;

  megaCfg->numldrv = enquiry3Pnt->numLDrv;
  megaCfg->max_cmds = megaCfg->productInfo.MaxConcCmds;
  if(megaCfg->max_cmds > MAX_COMMANDS) megaCfg->max_cmds = MAX_COMMANDS - 1;

  megaCfg->host->can_queue   = megaCfg->max_cmds;

  if (megaCfg->host->can_queue >= MAX_COMMANDS) {
    megaCfg->host->can_queue = MAX_COMMANDS-1;
  }

#ifdef HP			/* use HP firmware and bios version encoding */
  sprintf (megaCfg->fwVer, "%c%d%d.%d%d",
	   megaCfg->productInfo.FwVer[2],
	   megaCfg->productInfo.FwVer[1] >> 8,
	   megaCfg->productInfo.FwVer[1] & 0x0f,
	   megaCfg->productInfo.FwVer[2] >> 8,
	   megaCfg->productInfo.FwVer[2] & 0x0f);
  sprintf (megaCfg->biosVer, "%c%d%d.%d%d",
	   megaCfg->productInfo.BiosVer[2],
	   megaCfg->productInfo.BiosVer[1] >> 8,
	   megaCfg->productInfo.BiosVer[1] & 0x0f,
	   megaCfg->productInfo.BiosVer[2] >> 8,
	   megaCfg->productInfo.BiosVer[2] & 0x0f);
#else
	memcpy (megaCfg->fwVer, (void *)megaCfg->productInfo.FwVer, 4);
	megaCfg->fwVer[4] = 0;

	memcpy (megaCfg->biosVer, (void *)megaCfg->productInfo.BiosVer, 4);
	megaCfg->biosVer[4] = 0;
#endif

	printk ("megaraid: [%s:%s] detected %d logical drives" CRLFSTR,
        	megaCfg->fwVer,
		megaCfg->biosVer,
		megaCfg->numldrv);

	return 0;
}

/*-------------------------------------------------------------------------
 *
 *                      Driver interface functions
 *
 *-------------------------------------------------------------------------*/

/*----------------------------------------------------------
 * Returns data to be displayed in /proc/scsi/megaraid/X
 *----------------------------------------------------------*/
int megaraid_proc_info (char *buffer, char **start, off_t offset,
		    int length, int host_no, int inout)
{
  *start = buffer;
  return 0;
}

int mega_findCard (Scsi_Host_Template * pHostTmpl,
	  u16 pciVendor, u16 pciDev,
	  long flag)
{
  mega_host_config *megaCfg;
  struct Scsi_Host *host;
  u_char megaIrq;
  u32 megaBase;
  u16 numFound = 0;

  struct pci_dev *pdev = NULL;
  
  while ((pdev = pci_find_device (pciVendor, pciDev, pdev))) {
    if (pci_enable_device(pdev))
    	continue;
    if ((flag & BOARD_QUARTZ) && (skip_id == -1)) {
      u16 magic;
      pci_read_config_word(pdev, PCI_CONF_AMISIG, &magic);
      if ((magic != AMI_SIGNATURE) && (magic != AMI_SIGNATURE_471))
	continue;		/* not an AMI board */
    }
    printk (KERN_INFO "megaraid: found 0x%4.04x:0x%4.04x: in %s\n",
	    pciVendor,
	    pciDev,
	    pdev->slot_name);

    /* Read the base port and IRQ from PCI */
    megaBase = pci_resource_start (pdev, 0);
    megaIrq  = pdev->irq;

    if (flag & BOARD_QUARTZ)
      megaBase = (long) ioremap (megaBase, 128);
    else
      megaBase += 0x10;

    /* Initialize SCSI Host structure */
    host = scsi_register (pHostTmpl, sizeof (mega_host_config));
    if(host == NULL)
    	continue;
    megaCfg = (mega_host_config *) host->hostdata;
    memset (megaCfg, 0, sizeof (mega_host_config));

    printk ("scsi%d : Found a MegaRAID controller at 0x%x, IRQ: %d" CRLFSTR,
	    host->host_no, (u_int) megaBase, megaIrq);

    /* Copy resource info into structure */
    megaCfg->qCompletedH = NULL;
    megaCfg->qCompletedT = NULL;
    megaCfg->qPendingH = NULL;
    megaCfg->qPendingT = NULL;
    megaCfg->qFreeH    = NULL;
    megaCfg->qFreeT    = NULL;
    megaCfg->qFcnt = 0;
    megaCfg->qPcnt = 0;
    megaCfg->qCcnt = 0;
    megaCfg->flag = flag;
    megaCfg->host = host;
    megaCfg->base = megaBase;
    megaCfg->host->irq = megaIrq;
    megaCfg->host->io_port = megaBase;
    megaCfg->host->n_io_port = 16;
    megaCfg->host->unique_id = (pdev->bus->number << 8) | pdev->devfn;
    megaCtlrs[numCtlrs++] = megaCfg; 
    if (flag != BOARD_QUARTZ) {
      /* Request our IO Range */
      if (request_region (megaBase, 16, "megaraid")) {
	printk (KERN_WARNING "megaraid: Couldn't register I/O range!" CRLFSTR);
	scsi_unregister (host);
	continue;
      }
    }

    /* Request our IRQ */
    if (request_irq (megaIrq, megaraid_isr, SA_SHIRQ,
		     "megaraid", megaCfg)) {
      printk (KERN_WARNING "megaraid: Couldn't register IRQ %d!" CRLFSTR,
	      megaIrq);
      scsi_unregister (host);
      continue;
    }

    mega_register_mailbox (megaCfg, virt_to_bus ((void *) &megaCfg->mailbox64));
    mega_i_query_adapter (megaCfg);
   
    if (flag == BOARD_QUARTZ) {
      /* Check to see if this is a Dell PERC RAID controller model 466 */
      u16 subsysid, subsysvid;
#if LINUX_VERSION_CODE < 0x20100
      pcibios_read_config_word (pciBus, pciDevFun,
				PCI_SUBSYSTEM_VENDOR_ID,
				&subsysvid);
      pcibios_read_config_word (pciBus, pciDevFun,
				PCI_SUBSYSTEM_ID,
				&subsysid);
#else
      pci_read_config_word (pdev, PCI_SUBSYSTEM_VENDOR_ID, &subsysvid);
      pci_read_config_word (pdev, PCI_SUBSYSTEM_ID, &subsysid);
#endif
      if ( (subsysid == 0x1111) && (subsysvid == 0x1111) &&
           (!strcmp(megaCfg->fwVer,"3.00") || !strcmp(megaCfg->fwVer,"3.01"))) {
	printk(KERN_WARNING
"megaraid: Your card is a Dell PERC 2/SC RAID controller with firmware\n"
"megaraid: 3.00 or 3.01.  This driver is known to have corruption issues\n"
"megaraid: with those firmware versions on this specific card.  In order\n"
"megaraid: to protect your data, please upgrade your firmware to version\n"
"megaraid: 3.10 or later, available from the Dell Technical Support web\n"
"megaraid: site at\n"
"http://support.dell.com/us/en/filelib/download/index.asp?fileid=2940\n");
	megaraid_release (host);
#ifdef MODULE	
	continue;
#else
	while(1) schedule_timeout(1 * HZ);
#endif	
      }
    }

    /* Initialize SCBs */
    if (mega_initSCB (megaCfg)) {
      megaraid_release (host);
      continue;
    }

    numFound++;
  }
  return numFound;
}

/*---------------------------------------------------------
 * Detects if a megaraid controller exists in this system
 *---------------------------------------------------------*/
int megaraid_detect (Scsi_Host_Template * pHostTmpl)
{
  int count = 0;

#ifdef MODULE
  if (megaraid)
      megaraid_setup(megaraid);
#endif

  pHostTmpl->proc_name = "megaraid";

  printk ("megaraid: " MEGARAID_VERSION CRLFSTR);

  count += mega_findCard (pHostTmpl, 0x101E, 0x9010, 0);
  count += mega_findCard (pHostTmpl, 0x101E, 0x9060, 0);
  count += mega_findCard (pHostTmpl, 0x8086, 0x1960, BOARD_QUARTZ);

  return count;
}

/*---------------------------------------------------------------------
 * Release the controller's resources
 *---------------------------------------------------------------------*/
int megaraid_release (struct Scsi_Host *pSHost)
{
  mega_host_config *megaCfg;
  mega_mailbox *mbox;
  u_char mboxData[16];

  megaCfg = (mega_host_config *) pSHost->hostdata;
  mbox = (mega_mailbox *) mboxData;

  /* Flush cache to disk */
  memset (mbox, 0, 16);
  mboxData[0] = 0xA;

  free_irq (megaCfg->host->irq, megaCfg);/* Must be freed first, otherwise
					   extra interrupt is generated */

  /* Issue a blocking (interrupts disabled) command to the card */
  megaIssueCmd (megaCfg, mboxData, NULL, 0);

  /* Free our resources */
  if (megaCfg->flag & BOARD_QUARTZ) {
    iounmap ((void *) megaCfg->base);
  }
  else {
    release_region (megaCfg->host->io_port, 16);
  }

  mega_freeSgList(megaCfg);
  scsi_unregister (pSHost);

  return 0;
}

static inline void mega_freeSgList(mega_host_config *megaCfg)
{
  int i;

  for (i = 0; i < megaCfg->max_cmds; i++) {
    if (megaCfg->scbList[i].sgList)
      kfree (megaCfg->scbList[i].sgList);	/* free sgList */
  }
}

/*----------------------------------------------
 * Get information about the card/driver 
 *----------------------------------------------*/
const char * megaraid_info (struct Scsi_Host *pSHost)
{
  static char buffer[512];
  mega_host_config *megaCfg;

  megaCfg = (mega_host_config *) pSHost->hostdata;

  sprintf (buffer, "AMI MegaRAID %s %d commands %d targs %d chans %d luns",
	   megaCfg->fwVer,
           megaCfg->productInfo.MaxConcCmds,
	   megaCfg->host->max_id,
	   megaCfg->host->max_channel,
           megaCfg->host->max_lun);
  return buffer;
}

/*-----------------------------------------------------------------
 * Perform a SCSI command
 * Mailbox area:
 *   00 01 command
 *   01 01 command id
 *   02 02 # of sectors
 *   04 04 logical bus address
 *   08 04 physical buffer address
 *   0C 01 logical drive #
 *   0D 01 length of scatter/gather list
 *   0E 01 reserved
 *   0F 01 mailbox busy
 *   10 01 numstatus byte
 *   11 01 status byte 
 *-----------------------------------------------------------------*/
int megaraid_queue (Scsi_Cmnd * SCpnt, void (*pktComp) (Scsi_Cmnd *))
{
  DRIVER_LOCK_T
  mega_host_config *megaCfg;
  mega_scb *pScb;

  megaCfg = (mega_host_config *) SCpnt->host->hostdata;
  DRIVER_LOCK(megaCfg);

  if (!(megaCfg->flag & (1L << SCpnt->channel))) {
    if (SCpnt->channel < SCpnt->host->max_channel)
       printk (/*KERN_INFO*/ "scsi%d: scanning channel %c for devices.\n",
	    megaCfg->host->host_no,
	    SCpnt->channel + '1');
    else
       printk(/*KERN_INFO*/ "scsi%d: scanning virtual channel for logical drives.\n", megaCfg->host->host_no);
       
    megaCfg->flag |= (1L << SCpnt->channel);
  }

  SCpnt->scsi_done = pktComp;

  /* If driver in abort or reset.. cancel this command */
  if (megaCfg->flag & IN_ABORT) {
    SCpnt->result = (DID_ABORT << 16);
    /* Add Scsi_Command to end of completed queue */
    if( megaCfg->qCompletedH == NULL ) {
      megaCfg->qCompletedH = megaCfg->qCompletedT = SCpnt;
    }
    else {
      megaCfg->qCompletedT->host_scribble = (unsigned char *) SCpnt;
      megaCfg->qCompletedT = SCpnt;
    }
    megaCfg->qCompletedT->host_scribble = (unsigned char *) NULL;
    megaCfg->qCcnt++;

    DRIVER_UNLOCK(megaCfg);
    return 0;
  }
  else if (megaCfg->flag & IN_RESET) {
    SCpnt->result = (DID_RESET << 16);
    /* Add Scsi_Command to end of completed queue */
    if( megaCfg->qCompletedH == NULL ) {
      megaCfg->qCompletedH = megaCfg->qCompletedT = SCpnt;
    }
    else {
      megaCfg->qCompletedT->host_scribble = (unsigned char *) SCpnt;
      megaCfg->qCompletedT = SCpnt;
    }
    megaCfg->qCompletedT->host_scribble = (unsigned char *) NULL;
    megaCfg->qCcnt++;

    DRIVER_UNLOCK(megaCfg);
    return 0;
  }

  megaCfg->flag |= IN_QUEUE;
  /* Allocate and build a SCB request */
  if ((pScb = mega_build_cmd (megaCfg, SCpnt)) != NULL) {
              /*build SCpnt for IOCTL_CMD_NEW cmd in mega_ioctl()*/
    /* Add SCB to the head of the pending queue */
    /* Add SCB to the head of the pending queue */
    if( megaCfg->qPendingH == NULL ) {
      megaCfg->qPendingH = megaCfg->qPendingT = pScb;
    }
    else {
      megaCfg->qPendingT->next = pScb;
      megaCfg->qPendingT = pScb;
    }
    megaCfg->qPendingT->next = NULL;
    megaCfg->qPcnt++;

      mega_runpendq(megaCfg);

#if LINUX_VERSION_CODE > 0x020024
    if ( SCpnt->cmnd[0]==IOCTL_CMD_NEW )
    {  /* user data from external user buffer */
          char *user_area;
          u32  xfer_size;

          init_MUTEX_LOCKED(&pScb->sem);
          down(&pScb->sem);

          user_area = *((char **)&pScb->SCpnt->cmnd[4]);
          xfer_size = *((u32 *)&pScb->SCpnt->cmnd[8]);

          copy_to_user(user_area,pScb->kern_area,xfer_size);

          kfree(pScb->kern_area);

          mega_freeSCB(megaCfg, pScb);
    }
#endif
  }

  megaCfg->flag &= ~IN_QUEUE;
  DRIVER_UNLOCK(megaCfg);

  return 0;
}

/*----------------------------------------------------------------------
 * Issue a blocking command to the controller
 *----------------------------------------------------------------------*/
volatile static int internal_done_flag = 0;
volatile static int internal_done_errcode = 0;
static DECLARE_WAIT_QUEUE_HEAD(internal_wait);

static void internal_done (Scsi_Cmnd * SCpnt)
{
  internal_done_errcode = SCpnt->result;
  internal_done_flag++;
  wake_up(&internal_wait);
}

/* shouldn't be used, but included for completeness */

int megaraid_command (Scsi_Cmnd * SCpnt)
{
  internal_done_flag = 0;

  /* Queue command, and wait until it has completed */
  megaraid_queue (SCpnt, internal_done);

  while (!internal_done_flag) {
	interruptible_sleep_on(&internal_wait);
  }

  return internal_done_errcode;
}

/*---------------------------------------------------------------------
 * Abort a previous SCSI request
 *---------------------------------------------------------------------*/
int
megaraid_abort (Scsi_Cmnd * SCpnt)
{
  mega_host_config *megaCfg;
  int   rc; //, idx;
  mega_scb *pScb;

  rc = SCSI_ABORT_NOT_RUNNING;

  megaCfg = (mega_host_config *) SCpnt->host->hostdata;

  megaCfg->flag |= IN_ABORT;

  for(pScb=megaCfg->qPendingH; pScb; pScb=pScb->next) {
    if (pScb->SCpnt == SCpnt) {
      /* Found an aborting command */
#if DEBUG
      showMbox(pScb);
#endif

/*
 * If the command is queued to be issued to the firmware, abort the scsi cmd,
 * If the command is already aborted in a previous call to the _abort entry
 *  point, return SCSI_ABORT_SNOOZE, suggesting a reset.
 * If the command is issued to the firmware, which might complete after
 *  some time, we will mark the scb as aborted, and return to the mid layer, 
 *  that abort could not be done.
 *  In the ISR, when this command actually completes, we will perform a normal
 *  completion.
 *
 * Oct 27, 1999
 */

      switch(pScb->state) {
      case SCB_ABORTED: /* Already aborted */
	rc = SCSI_ABORT_SNOOZE;
	break;
      case SCB_ISSUED: /* Waiting on ISR result */
	rc = SCSI_ABORT_NOT_RUNNING;
	pScb->state = SCB_ABORTED;
	break;
      case SCB_ACTIVE: /* still on the pending queue */
	mega_freeSCB (megaCfg, pScb);
	SCpnt->result = (DID_ABORT << 16) ;
        if( megaCfg->qCompletedH == NULL ) {
          megaCfg->qCompletedH = megaCfg->qCompletedT = SCpnt;
        }
        else {
          megaCfg->qCompletedT->host_scribble = (unsigned char *) SCpnt;
          megaCfg->qCompletedT = SCpnt;
        }
        megaCfg->qCompletedT->host_scribble = (unsigned char *) NULL;
        megaCfg->qCcnt++;
	rc = SCSI_ABORT_SUCCESS;
        break;
      default:
        printk("megaraid_abort: unknown command state!!\n");
        rc = SCSI_ABORT_NOT_RUNNING;
	break;
      }
      break;
    }
  }

  megaCfg->flag &= ~IN_ABORT;

#if DEBUG
if(megaCfg->flag & IN_QUEUE)  printk("ma:flag is in queue\n");
if(megaCfg->qCompletedH == NULL) printk("ma:qchead == null\n");
#endif
  
/*
 * This is required here to complete any completed requests to be communicated
 * over to the mid layer.
 * Calling just mega_rundoneq() did not work.
 */
if(megaCfg->qCompletedH) {
  SCpnt = megaCfg->qCompletedH;
  megaCfg->qCompletedH = (Scsi_Cmnd *)SCpnt->host_scribble;
  megaCfg->qCcnt--;

  SCpnt->host_scribble = (unsigned char *) NULL ;
  /* Callback */
  callDone (SCpnt);
}
  mega_rundoneq(megaCfg);

  return rc;
}

/*---------------------------------------------------------------------
 * Reset a previous SCSI request
 *---------------------------------------------------------------------*/
int megaraid_reset (Scsi_Cmnd * SCpnt, unsigned int rstflags)
{
  mega_host_config *megaCfg;
  int idx;
  int rc;
  mega_scb *pScb;

  rc = SCSI_RESET_NOT_RUNNING;
  megaCfg = (mega_host_config *) SCpnt->host->hostdata;

  megaCfg->flag |= IN_RESET;

  printk ("megaraid_RESET: %.08lx cmd=%.02x <c=%d.t=%d.l=%d>, flag = %x\n",
	SCpnt->serial_number, SCpnt->cmnd[0], SCpnt->channel, SCpnt->target,
	  SCpnt->lun, rstflags);

  TRACE (("RESET: %.08lx %.02x <%d.%d.%d>\n",
	SCpnt->serial_number, SCpnt->cmnd[0], SCpnt->channel, SCpnt->target,
	  SCpnt->lun));

  /*
   * Walk list of SCBs for any that are still outstanding
   */
  for (idx = 0; idx < megaCfg->max_cmds; idx++) {
    if (megaCfg->scbList[idx].state != SCB_FREE) {
      SCpnt = megaCfg->scbList[idx].SCpnt;
      pScb = &megaCfg->scbList[idx];
      if (SCpnt != NULL) {
        pScb->state = SCB_RESET;
        break;
      }
    }
  }

  megaCfg->flag &= ~IN_RESET;

  mega_rundoneq(megaCfg);
  return rc;
}

/*-------------------------------------------------------------
 * Return the disk geometry for a particular disk
 * Input:
 *   Disk *disk - Disk geometry
 *   kdev_t dev - Device node
 *   int *geom  - Returns geometry fields
 *     geom[0] = heads
 *     geom[1] = sectors
 *     geom[2] = cylinders
 *-------------------------------------------------------------*/
int megaraid_biosparam (Disk * disk, kdev_t dev, int *geom)
{
  int heads, sectors, cylinders;
  mega_host_config *megaCfg;

  /* Get pointer to host config structure */
  megaCfg = (mega_host_config *) disk->device->host->hostdata;

  /* Default heads (64) & sectors (32) */
  heads = 64;
  sectors = 32;
  cylinders = disk->capacity / (heads * sectors);

  /* Handle extended translation size for logical drives > 1Gb */
  if (disk->capacity >= 0x200000) {
    heads = 255;
    sectors = 63;
    cylinders = disk->capacity / (heads * sectors);
  }

  /* return result */
  geom[0] = heads;
  geom[1] = sectors;
  geom[2] = cylinders;

  return 0;
}

static int __init megaraid_setup(char *str)
{
  skip_id = -1;
  if (str && !strncmp(str, "skip", strlen("skip"))) {
      if (str[4] != '\0') {
          skip_id = str[4] - '0';
          if (str[5] != '\0') {
              skip_id = (skip_id * 10) + (str[5] - '0');
          }
      }
      skip_id = (skip_id > 15) ? -1 : skip_id;
  }
  return 1;
}

__setup("megaraid=", megaraid_setup);

static Scsi_Host_Template driver_template = MEGARAID;

#include "scsi_module.c"
