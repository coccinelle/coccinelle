/*
 * NinjaSCSI-32Bi Cardbus, NinjaSCSI-32UDE PCI/CardBus SCSI driver
 * Copyright (C) 2001, 2002
 *      YOKOTA Hiroshi <yokota@netlab.is.tsukuba.ac.jp>
 *      GOTO Masanori <gotom@debian.or.jp>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

#include <linux/version.h>
#include <linux/module.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/slab.h>
#include <linux/string.h>
#include <linux/timer.h>
#include <linux/ioport.h>
#include <linux/major.h>
#include <linux/blk.h>
#include <linux/interrupt.h>
#include <linux/pci.h>
#include <linux/delay.h>
#include <linux/ctype.h>

#include <asm/dma.h>
#include <asm/system.h>
#include <asm/io.h>

#include "scsi.h"
#include "hosts.h"
#include <scsi/scsi_ioctl.h>
#include <scsi/scsi.h>

#include "nsp32.h"


static int trans_mode = 0;	/* default: BIOS */
static int auto_param = 0;	/* default: ON */
MODULE_PARM(trans_mode, "i");
MODULE_PARM(auto_param, "i");
MODULE_PARM_DESC(trans_mode, "transfer mode (0: BIOS 1: Async 2: Ultra20M");
MODULE_PARM_DESC(auto_param, "AutoParameter mode (0: ON 1: OFF)");
#define ASYNC_MODE    1
#define ULTRA20M_MODE 2

MODULE_AUTHOR("YOKOTA Hiroshi <yokota@netlab.is.tsukuba.ac.jp>, GOTO Masanori <gotom@debian.or.jp>");
MODULE_DESCRIPTION("Workbit NinjaSCSI-32Bi/UDE PCI/CardBus SCSI host bus adapter module");
MODULE_LICENSE("GPL");

static const char *nsp32_release_version = "1.0";


/*
 * structure for DMA/Scatter Gather list
 */
#define AUTOPARAM_SIZE		(sizeof(int)*0x15)	/* 4x15H = 0x60 */
#define NSP_SG_SIZE		SG_ALL
#define NSP32_SG_END_SGT	0x80000000		/* Last SGT marker */
#define NSP32_SG_CNT_MASK	0x1FFFF

struct nsp32_sgtable {
	unsigned long		addr;		/* transfer address */
	unsigned long		len;		/* transfer length.
						   Bit (24-32) is for SGT_END */
};

struct nsp32_sglun {
	struct nsp32_sgtable sgt[NSP_SG_SIZE+1];	/* SG table */
};


/*
 * host data structure
 */
/* message in/out buffer */
#define MSGOUTBUF_MAX	13  /* 13 is ok ? */
#define MSGINBUF_MAX	13

/* flag for trans_method */
#define NSP32_TRANSFER_BUSMASTER	BIT(0)
#define NSP32_TRANSFER_MMIO		BIT(1)	/* Not supported yet */
#define NSP32_TRANSFER_PIO		BIT(2)	/* Not supported yet */


/*
 * SCSI TARGET/LUN definition
 */
#define NSP32_HOST_SCSIID	7	/* SCSI initiator is everytime defined as 7 */
#define MAX_TARGET		8
#define MAX_LUN			8	/* XXX: In SPI3, max number of LUN is 64. */


/*
 * structure for synchronous transfer negotiation data
 */
#define SYNC_NOT_YET	0
#define SYNC_OK		1
#define SYNC_NG		2

struct nsp32_sync_table {
	unsigned char	period_num;	/* period number */
	unsigned char	ackwidth;	/* ack width designated by period */
	unsigned char	start_period;	/* search range - start period */
	unsigned char	end_period;	/* search range - end period */
};


/*
 * structure for target device static data
 */
/* flag for nsp32_target.sync_flag */
#define SDTR_INITIATOR		BIT(0)	/* sending SDTR from initiator */
#define SDTR_TARGET		BIT(1)	/* sending SDTR from target */
#define SDTR_DONE		BIT(2)	/* exchanging SDTR has been processed */

/* syncronous period value for nsp32_target.config_max */
#define FAST5M			0x32
#define FAST10M			0x19
#define ULTRA20M		0x0c

/* flag for nsp32_target.{sync_offset}, period */
#define ASYNC_OFFSET		0	/* asynchronous transfer */
#define SYNC_OFFSET		0xf	/* synchronous transfer max offset */

/* syncreg:
      07 06 05 04 03 02 01 00
      ---PERIOD-- ---OFFSET--   */
#define TO_SYNCREG(period, offset)	(period << 4 | offset)

struct nsp32_target {
	unsigned char	syncreg;	/* value for SYNCREG  */
	unsigned char	ackwidth;	/* value for ACKWIDTH */
	unsigned char	offset;		/* sync offset (0-15) */
	int		sync_flag;	/* SDTR_*, 0 */
	int		limit_entry;	/* max speed limit entry designated
					   by EEPROM configuration */
};

typedef struct _nsp32_hw_data {
	int IrqNumber;
	int BaseAddress;
	int NumAddress;
#define NSP32_MMIO_OFFSET 0x0800
	unsigned long MmioAddress;
	unsigned long length;

	Scsi_Cmnd *CurrentSC;

	struct pci_dev *Pci;
	const struct pci_device_id *pci_devid;
	struct Scsi_Host *Host;
	spinlock_t Lock;

	char info_str[100];

	/* allocated memory region */
	struct nsp32_lunt	*lunt_list;	/* kmalloc region for lunt */
	struct nsp32_sglun	*sg_list;	/* sglist virtual address */
	dma_addr_t		sgaddr;		/* physical address of hw_sg_table */
	unsigned char		*autoparam;	/* auto parameter transfer region */
	dma_addr_t		apaddr;		/* physical address of autoparam */
	int 			cur_entry;	/* current sgt entry */

	/* target/LUN */
	struct nsp32_lunt	*curlunt;	/* Current connected LUN table */
	struct nsp32_lunt	*lunt[MAX_TARGET][MAX_LUN];  /* All LUN table */
	struct nsp32_target	*curtarget;	/* Current connected SCSI ID */
	struct nsp32_target	target[MAX_TARGET];	     /* SCSI ID */
	int			pid;		/* Current connected target ID */
	int			plun;		/* Current connected target LUN */

	/* behavior setting parameters */
	int			trans_method;	/* transfer method flag */
	int			resettime;	/* Reset time */
	int 			clock;	       	/* clock dividing flag */
	struct nsp32_sync_table	*synct;		/* sync_table determined by clock */
	int			syncnum;	/* the max number of synct element */

	/* message buffer */
        unsigned char	msgoutbuf[MSGOUTBUF_MAX]; /* msgout buffer */
	char		msgoutlen;		  /* msgoutbuf length */
	unsigned char	msginbuf[MSGINBUF_MAX];	  /* megin buffer */
	char		msginlen;		  /* msginbuf length */


} nsp32_hw_data;
static nsp32_hw_data nsp32_data_base;  /* probe <-> detect glue */


/*
 * TIME definition
 */
#define RESET_HOLD_TIME		10000	/* reset time in us (SCSI-2 says the
					   minimum is 25us) */
#define SEL_TIMEOUT_TIME	10000	/* 250ms defined in SCSI specification
					   (25.6us/1unit) */
#define ARBIT_TIMEOUT_TIME	100	/* 100us */
#define REQSACK_TIMEOUT_TIME	10000	/* max wait time for REQ/SACK assertion
					   or negation, 10000us == 10ms */

/*
 * structure for connected LUN dynamic data
 *
 * Note: Currently tagged queuing is disabled, each nsp32_lunt holds
 *       one SCSI command and one state.
 */
#define DISCPRIV_OK		BIT(0)		/* DISCPRIV Enable mode */
#define MSGIN03			BIT(1)		/* Auto Msg In 03 Flag */

struct nsp32_lunt {
	Scsi_Cmnd		*SCpnt;		/* Current Handling Scsi_Cmnd */
	unsigned long		save_datp;	/* Save Data Pointer - saved position from initial address */
	int			msgin03;	/* auto msg in 03 flag */
	unsigned int		sg_num;		/* Total number of SG entries */
	int			cur_entry;	/* Current SG entry number */
	struct nsp32_sglun	*sglun;		/* sg table per lun */
	long			sglun_paddr;	/* sglun physical address */
};


/*
 * Period/AckWidth speed conversion table
 *
 * Note: This period/ackwidth speed table must be in descending order.
 */
static struct nsp32_sync_table nsp32_sync_table_40M[] = {
     /* {PNo, AW,  SP,   EP}  Speed(MB/s) Period AckWidth */
	{0x1, 0, 0x0c, 0x0c},	/*  20.0 :  50ns,  25ns */
	{0x2, 0, 0x0d, 0x18},	/*  13.3 :  75ns,  25ns */
	{0x3, 1, 0x19, 0x19},	/*  10.0 : 100ns,  50ns */
	{0x4, 1, 0x1a, 0x1f},	/*   8.0 : 125ns,  50ns */
	{0x5, 2, 0x20, 0x25},	/*   6.7 : 150ns,  75ns */
	{0x6, 2, 0x26, 0x31},	/*   5.7 : 175ns,  75ns */
	{0x7, 3, 0x32, 0x32},	/*   5.0 : 200ns, 100ns */
	{0x8, 3, 0x33, 0x38},	/*   4.4 : 225ns, 100ns */
	{0x9, 3, 0x39, 0x3e},	/*   4.0 : 250ns, 100ns */
};
static const int nsp32_table_40M_num =
		sizeof(nsp32_sync_table_40M)/sizeof(struct nsp32_sync_table);

static struct nsp32_sync_table nsp32_sync_table_20M[] = {
	{0x1, 0, 0x19, 0x19},	/* 10.0 : 100ns,  50ns */
	{0x2, 0, 0x1a, 0x25},	/*  6.7 : 150ns,  50ns */
	{0x3, 1, 0x26, 0x32},	/*  5.0 : 200ns, 100ns */
	{0x4, 1, 0x33, 0x3e},	/*  4.0 : 250ns, 100ns */
	{0x5, 2, 0x3f, 0x4b},	/*  3.3 : 300ns, 150ns */
	{0x6, 2, 0x4c, 0x57},	/*  2.8 : 350ns, 150ns */
	{0x7, 3, 0x58, 0x64},	/*  2.5 : 400ns, 200ns */
	{0x8, 3, 0x65, 0x70},	/*  2.2 : 450ns, 200ns */
	{0x9, 3, 0x71, 0x7d},	/*  2.0 : 500ns, 200ns */
};
static const int nsp32_table_20M_num =
		sizeof(nsp32_sync_table_20M)/sizeof(struct nsp32_sync_table);

static struct nsp32_sync_table nsp32_sync_table_pci[] = {
	{0x1, 0, 0x0c, 0x0f},	/* 16.6 :  60ns,  30ns */
	{0x2, 0, 0x10, 0x16},	/* 11.1 :  90ns,  30ns */
	{0x3, 1, 0x17, 0x1e},	/*  8.3 : 120ns,  60ns */
	{0x4, 1, 0x1f, 0x25},	/*  6.7 : 150ns,  60ns */
	{0x5, 2, 0x26, 0x2d},	/*  5.6 : 180ns,  90ns */
	{0x6, 2, 0x2e, 0x34},	/*  4.8 : 210ns,  90ns */
	{0x7, 3, 0x35, 0x3c},	/*  4.2 : 240ns, 120ns */
	{0x8, 3, 0x3d, 0x43},	/*  3.7 : 270ns, 120ns */
	{0x9, 3, 0x44, 0x4b}, 	/*  3.3 : 300ns, 120ns */
};
static const int nsp32_table_pci_num =
		sizeof(nsp32_sync_table_pci)/sizeof(struct nsp32_sync_table);

/*
 * function declaration
 */
static int nsp32_detect(Scsi_Host_Template *);
static int nsp32_queuecommand(Scsi_Cmnd *, void (*done)(Scsi_Cmnd *));
static const char *nsp32_info(struct Scsi_Host *);
static int nsp32_eh_abort(Scsi_Cmnd *);
static int nsp32_eh_bus_reset(Scsi_Cmnd *);
static int nsp32_eh_host_reset(Scsi_Cmnd *);
static int nsp32_reset(Scsi_Cmnd *, unsigned int);
static int nsp32_release(struct Scsi_Host *);
static int nsp32_proc_info(char *, char **, off_t, int, int, int);
static int __devinit nsp32_probe(struct pci_dev *, const struct pci_device_id *);
static void __devexit nsp32_remove(struct pci_dev *);
static int __init init_nsp32(void);
static void __exit exit_nsp32(void);

static void nsp32_message(const char *, int, char *, char *, ...);
static void nsp32_dmessage(const char *, int, int, char *, ...);
static void nsp32_build_identify(nsp32_hw_data *, Scsi_Cmnd *);
static void nsp32_build_sdtr(nsp32_hw_data *, unsigned char, unsigned char);
static void nsp32_build_nop(nsp32_hw_data *);
static void nsp32_build_reject(nsp32_hw_data *);
static int nsp32hw_start_selection(Scsi_Cmnd *, nsp32_hw_data *);
static int nsp32_selection_autoscsi(Scsi_Cmnd *, nsp32_hw_data *);
static int nsp32_reselection(nsp32_hw_data *, unsigned char);
static int nsp32hw_setup_sg_table(Scsi_Cmnd *, nsp32_hw_data *);
static int nsp32hw_init(struct Scsi_Host *);
static void nsp32_scsi_done(nsp32_hw_data *, Scsi_Cmnd *);
static int nsp32_busfree_occur(nsp32_hw_data *, unsigned short);
static void nsp32_adjust_busfree(nsp32_hw_data *, unsigned int);
static void nsp32_msgout_occur(nsp32_hw_data *);
static void nsp32_restart_autoscsi(nsp32_hw_data *, unsigned short);
static void nsp32_msgin_occur(nsp32_hw_data *, unsigned long, unsigned short);
static void nsp32_analyze_sdtr(nsp32_hw_data *);
static int nsp32_search_period_entry(nsp32_hw_data *,struct nsp32_target *, unsigned char);
static void nsp32_set_async(nsp32_hw_data *, struct nsp32_target *);
static void nsp32_set_max_sync(nsp32_hw_data *, struct nsp32_target *, unsigned char *, unsigned char *);
static void nsp32_set_sync_entry(nsp32_hw_data *, struct nsp32_target *, int, unsigned char);
static void nsp32_wait_req(nsp32_hw_data *, int);
static void nsp32_wait_sack(nsp32_hw_data *, int);
static void nsp32_sack_assert(nsp32_hw_data *);
static void nsp32_sack_negate(nsp32_hw_data *);
static void nsp32_do_bus_reset(nsp32_hw_data *);

static int nsp32_getprom_param(nsp32_hw_data *);
static int nsp32_getprom_new(nsp32_hw_data *);
static int nsp32_getprom_standard(nsp32_hw_data *);
static int nsp32_prom_read(nsp32_hw_data *, int);
static void nsp32_prom_start(nsp32_hw_data *);
static void nsp32_prom_stop(nsp32_hw_data *);
static void nsp32_prom_write(nsp32_hw_data *, int);
static int nsp32_prom_fetch(nsp32_hw_data *);
static inline void nsp32_prom_set(nsp32_hw_data *, int, int);
static inline int nsp32_prom_get(nsp32_hw_data *, int);


/*
 * max_sectors is currently limited up to 128.
 */
static Scsi_Host_Template driver_template = {
	.proc_name =			"nsp32",
	.name =				"Workbit NinjaSCSI-32Bi/UDE",
	.proc_info =			nsp32_proc_info,
	.detect =			nsp32_detect,
	.info =				nsp32_info,
	.queuecommand =			nsp32_queuecommand,
	.can_queue =			1,
	.sg_tablesize =			NSP_SG_SIZE,
	.max_sectors =			128,
	.cmd_per_lun =			1,
	.this_id =			7,
	.use_clustering =		DISABLE_CLUSTERING,
	.eh_abort_handler =       	nsp32_eh_abort,
	.eh_device_reset_handler =	NULL,
	.eh_bus_reset_handler =		nsp32_eh_bus_reset,
	.eh_host_reset_handler =	nsp32_eh_host_reset,
	.release =			nsp32_release,
#if (LINUX_VERSION_CODE < KERNEL_VERSION(2,5,2))
	.use_new_eh_code =        	1,
#else
	/* .highmem_io =		1,	*/
#endif
};

#include "nsp32_io.h"

/*
 * debug, error print
 */
#define nsp32_msg(type, args...) \
	nsp32_message(__FUNCTION__, __LINE__, (type), ## args)
#define nsp32_dbg(mask, args...) \
	nsp32_dmessage(__FUNCTION__, __LINE__, (mask), ## args)

#ifndef NSP32_DEBUG
# define NSP32_DEBUG_MASK		0x000000
#else
# define NSP32_DEBUG_MASK		0xffffff
#endif

#define NSP32_DEBUG_QUEUECOMMAND	0x000001
#define NSP32_DEBUG_REGISTER		0x000002
#define NSP32_DEBUG_AUTOSCSI		0x000004
#define NSP32_DEBUG_INTR		0x000008
#define NSP32_DEBUG_SGLIST		0x000010
#define NSP32_DEBUG_BUSFREE		0x000020
#define NSP32_DEBUG_CDB_CONTENTS	0x000040
#define NSP32_DEBUG_RESELECTION		0x000080
#define NSP32_DEBUG_MSGINOCCUR		0x000100
#define NSP32_DEBUG_EEPROM		0x000200
#define NSP32_DEBUG_MSGOUTOCCUR		0x000400
#define NSP32_DEBUG_BUSRESET		0x000800
#define NSP32_DEBUG_RESTART		0x001000
#define NSP32_DEBUG_SYNC		0x002000
#define NSP32_DEBUG_WAIT		0x004000
#define NSP32_DEBUG_TARGETFLAG		0x008000
#define NSP32_DEBUG_PROC		0x010000
#define NSP32_DEBUG_INIT		0x020000
#define NSP32_SPECIAL_PRINT_REGISTER	0x100000

#define NSP32_DEBUG_BUF_LEN		100

static void nsp32_message(const char *func, int line, char *type, char *fmt, ...)
{
	va_list args;
	char buf[NSP32_DEBUG_BUF_LEN];

	va_start(args, fmt);
	vsprintf(buf, fmt, args);
	va_end(args);

#ifndef NSP32_DEBUG
	printk("%snsp32: %s\n", type, buf);
#else
	printk("%snsp32: %s (%d): %s\n", type, func, line, buf);
#endif
}

static void nsp32_dmessage(const char *func, int line, int mask, char *fmt, ...)
{
	va_list args;
	char buf[NSP32_DEBUG_BUF_LEN];

	va_start(args, fmt);
	vsprintf(buf, fmt, args);
	va_end(args);

	if (mask & NSP32_DEBUG_MASK) {
		printk("Ninja: %d %s (%d): %s\n", mask, func, line, buf);
	}
}

#ifdef NSP32_DEBUG
# include "nsp32_debug.c"
#else
# define show_command(arg)   /* */
# define show_busphase(arg)  /* */
# define show_autophase(arg) /* */
#endif

#ifdef NSP32_DEBUG
static int pc_debug = NSP32_DEBUG;
MODULE_PARM(pc_debug, "i");
#define DEBUG(n, args...) if (pc_debug>(n)) printk(/*KERN_DEBUG*/ args)
#else
#define DEBUG(n, args...)
#endif


/*
 * IDENTIFY Message
 */
static void nsp32_build_identify(nsp32_hw_data *data, Scsi_Cmnd *SCpnt)
{
	int pos = data->msgoutlen;

	data->msgoutbuf[pos++] =
		0x80 |		/* Identify */
#if 0
		/* XXX: Auto DiscPriv detection is progressing... */
		0x40 |		/* DiscPriv */
#endif
		SCpnt->device->lun;	/* LUNTRN */

	data->msgoutlen = pos;
}

/*
 * SDTR Message Routine
 */
static void nsp32_build_sdtr(nsp32_hw_data *data,
			     unsigned char period, unsigned char offset)
{
	int pos = data->msgoutlen;

	data->msgoutbuf[pos++] = EXTENDED_MESSAGE;
	data->msgoutbuf[pos++] = EXTENDED_SDTR_LEN;
	data->msgoutbuf[pos++] = EXTENDED_SDTR;
	data->msgoutbuf[pos++] = period;
	data->msgoutbuf[pos++] = offset;

	data->msgoutlen = pos;
}

/*
 * No Operation Message
 */
static void nsp32_build_nop(nsp32_hw_data *data)
{
	int pos = data->msgoutlen;

	if (pos != 0) {
		nsp32_msg(KERN_WARNING,
			  "Some messages are already contained!");
		return;
	}

	data->msgoutbuf[pos++] = NOP;
	data->msgoutlen = pos;
}

/*
 * Reject Message
 */
static void nsp32_build_reject(nsp32_hw_data *data)
{
	int pos = data->msgoutlen;

	data->msgoutbuf[pos++] = MESSAGE_REJECT;
	data->msgoutlen = pos;
}
	
/*
 * timer
 */
#if 0
static void nsp32_start_timer(Scsi_Cmnd *SCpnt, int time)
{
	unsigned int base = SCpnt->host->io_port;

	DEBUG(0, __func__ " time=%d\n", time);

	if (time & (~TIMER_CNT_MASK)) {
		printk("timer set overflow\n");
	}

	nsp32_write2(base, TIMER_SET, time & TIMER_CNT_MASK);
}
#endif


/*
 * set SCSI command and other parameter to asic, and start selection phase
 */
static int nsp32hw_start_selection(Scsi_Cmnd *SCpnt, nsp32_hw_data *data)
{
	unsigned int   host_id = SCpnt->device->host->this_id;
	unsigned int   base    = SCpnt->device->host->io_port;
	unsigned char  target  = SCpnt->device->id;
	unsigned char  *param  = data->autoparam;
	unsigned char  phase, arbit;
	int	       i, time;
	unsigned int   msgout;
	unsigned long  l;
	unsigned short s;

	/*
	 * check bus free
	 */
	phase = nsp32_read1(base, SCSI_BUS_MONITOR);
	if (phase != BUSMON_BUS_FREE) {
		nsp32_msg(KERN_WARNING, "bus busy");
		show_busphase(phase & BUSMON_PHASE_MASK);
		SCpnt->result = DID_BUS_BUSY << 16;
		return FALSE;
	}

	/*
	 * message out
	 *
	 * Note: If the range of msgoutlen is 1 - 3, fill scsi_msgout.
	 *       over 3 messages needs another routine.
	 */
	if (data->msgoutlen == 0) {
		nsp32_msg(KERN_ERR, "SCSI MsgOut without any message!");
		SCpnt->result = DID_ERROR << 16;
		return FALSE;
	} else if (data->msgoutlen > 0 && data->msgoutlen <= 3) {
		msgout = 0;
		for (i=0; i<data->msgoutlen; i++) {
			/*
			 * the sending order of the message is:
			 *  MCNT 3: MSG#0 -> MSG#1 -> MSG#2
			 *  MCNT 2:          MSG#1 -> MSG#2
			 *  MCNT 1:                   MSG#2    
			 */
			msgout >>= 8;
			msgout |= (unsigned int)(data->msgoutbuf[i] << 24);
		}
		msgout |= MV_VALID;	/* MV valid */
		msgout |= (unsigned int)data->msgoutlen; /* len */
	} else {
		/* data->msgoutlen > 3 */
		msgout = 0;
	}

	/*
	 * setup asic parameter
	 */
	memset(param, 0, AUTOPARAM_SIZE);

	/* cdb */
	for (i=0; i<SCpnt->cmd_len; i++) {
		param[4*i] = SCpnt->cmnd[i];
	}

	/* message out */
	param[4*0x10 +0] = (msgout & 0x000000ff) >> 0;
	param[4*0x10 +1] = (msgout & 0x0000ff00) >> 8;
	param[4*0x10 +2] = (msgout & 0x00ff0000) >> 16;
	param[4*0x10 +3] = (msgout & 0xff000000) >> 24;

	/* syncreg, ackwidth, target id, sampleing rate */
	param[4*0x11 +0] = data->curtarget->syncreg;
	param[4*0x11 +1] = data->curtarget->ackwidth;
	param[4*0x11 +2] = BIT(host_id) | BIT(target);
	param[4*0x11 +3] = 0;

	/* command control */
	s = (CLEAR_CDB_FIFO_POINTER | AUTOSCSI_START |
	     AUTO_MSGIN_00_OR_04 | AUTO_MSGIN_02 | AUTO_ATN);
	param[4*0x12 +0] = (s & 0x00ff) >> 0;
	param[4*0x12 +1] = (s & 0xff00) >> 8;

	/* transfer control */
	s = 0;
	switch (data->trans_method) {
	case NSP32_TRANSFER_BUSMASTER:
		s |= BM_START;
		break;
	case NSP32_TRANSFER_MMIO:
		s |= CB_MMIO_MODE;
		break;
	case NSP32_TRANSFER_PIO:
		s |= CB_IO_MODE;
		break;
	default:
		nsp32_msg(KERN_ERR, "unknown trans_method");
	}
	/*
	 * ORed BLIEND_MODE, FIFO intr is decreased, instead of PCI bus waits.
	 * For bus master transfer, it's taken off.
	 */
	s |= (TRANSFER_GO | ALL_COUNTER_CLR);
	param[4*0x12 +2] = (s & 0x00ff) >> 0;
	param[4*0x12 +3] = (s & 0xff00) >> 8;

	/* sg table addr */
	l = data->curlunt->sglun_paddr;
	param[4*0x13 +0] = (l & 0x000000ff) >> 0;
	param[4*0x13 +1] = (l & 0x0000ff00) >> 8;
	param[4*0x13 +2] = (l & 0x00ff0000) >> 16;
	param[4*0x13 +3] = (l & 0xff000000) >> 24;

	/*
	 * transfer parameter to asic
	 */
	nsp32_write4(base, SGT_ADR,         virt_to_bus(param));
	nsp32_write2(base, COMMAND_CONTROL, CLEAR_CDB_FIFO_POINTER |
		                            AUTO_PARAMETER         );

	/*
	 * Arbitration Status Check
	 *	
	 * Note: Arbitration counter is wait during ARBIT_GO is not lifting.
	 *	 Using udelay(1) consumes CPU time and system time, but 
	 *	 arbitration delay time is defined minimal 2.4us in SCSI
	 *	 specification, thus udelay works as coarse grained wait timer.
	 */
	time = 0;
	do {
		arbit = nsp32_read1(base, ARBIT_STATUS);
		nsp32_dbg(NSP32_DEBUG_AUTOSCSI, "arbit=0x%x", arbit);
	} while ((arbit & (ARBIT_WIN | ARBIT_FAIL)) == 0 &&
		 (time++ <= 1000));

	nsp32_dbg(NSP32_DEBUG_AUTOSCSI,
		  "arbit: 0x%x, delay time: %d", arbit, time);

	if (arbit & ARBIT_WIN) {
		SCpnt->result = DID_OK << 16;
		/* PCI LED on! */
		nsp32_index_write1(base, EXT_PORT, LED_ON);
	} else if (arbit & ARBIT_FAIL) {
		SCpnt->result = DID_BUS_BUSY << 16;
		nsp32_write1(base, SET_ARBIT, ARBIT_CLEAR);
		return FALSE;
	} else {
		/* unknown error or ARBIT_GO timeout */
		nsp32_dbg(NSP32_DEBUG_AUTOSCSI, "arbit fail");
		SCpnt->result = DID_NO_CONNECT << 16;
		nsp32_write1(base, SET_ARBIT, ARBIT_CLEAR);
		return FALSE;
        }

	/*
	 * clear Arbit
	 */
	nsp32_write1(base, SET_ARBIT, ARBIT_CLEAR);

	return TRUE;
}


/*
 * Selection with AUTO SCSI (without AUTO PARAMETER)
 */
static int nsp32_selection_autoscsi(Scsi_Cmnd *SCpnt, nsp32_hw_data *data)
{
	unsigned char	phase;
	unsigned char	arbit;
	int		status;
	int		i;
	unsigned short	command	= 0;
	int		time = 0;
	unsigned int	msgout = 0;
	unsigned short	execph;
	unsigned int	base = data->BaseAddress;

	/*
	 * IRQ disable
	 */
	nsp32_write2(base, IRQ_CONTROL, IRQ_CONTROL_ALL_IRQ_MASK);

	/*
	 * check bus line
	 */
	phase = nsp32_read1(base, SCSI_BUS_MONITOR);
	if(((phase & BUSMON_BSY) == 1) ||
	   (phase & BUSMON_SEL) == 1) {
		nsp32_msg(KERN_WARNING, "bus busy");
		SCpnt->result = DID_BUS_BUSY << 16;
		status = 1;
		goto out;
        }

	/*
	 * clear execph
	 */
	execph = nsp32_read2(base, SCSI_EXECUTE_PHASE);

	/*
	 * clear FIFO counter to set CDBs
	 */
	nsp32_write2(base, COMMAND_CONTROL, CLEAR_CDB_FIFO_POINTER);

	/*
	 * set CDB0 - CDB15
	 */
	for (i=0; i<SCpnt->cmd_len; i++) {
		nsp32_write1(base, COMMAND_DATA, SCpnt->cmnd[i]);
        }
	nsp32_dbg(NSP32_DEBUG_CDB_CONTENTS, "CDB[0]=[0x%x]", SCpnt->cmnd[i]);

	/*
	 * set SCSIOUT LATCH(initiator)/TARGET(target) (ORed) ID
	 */
	nsp32_write1(base, SCSI_OUT_LATCH_TARGET_ID,
		((1 << NSP32_HOST_SCSIID) | (1 << SCpnt->device->id)));

	/*
	 * set SCSI MSGOUT REG
	 *
	 * Note: If the range of msgoutlen is 1 - 3, fill scsi_msgout.
	 *       over 3 messages needs another routine.
	 */
	if (data->msgoutlen == 0) {
		nsp32_msg(KERN_ERR, 
			  "SCSI MsgOut without any message!");
		SCpnt->result = DID_ERROR << 16;
		status = 1;
		goto out;
	} else if (data->msgoutlen > 0 && data->msgoutlen <= 3) {
		msgout = 0;
		for (i=0; i<data->msgoutlen; i++) {
			/*
			 * the sending order of the message is:
			 *  MCNT 3: MSG#0 -> MSG#1 -> MSG#2
			 *  MCNT 2:          MSG#1 -> MSG#2
			 *  MCNT 1:                   MSG#2    
			 */
			msgout >>= 8;
			msgout |= (unsigned int)(data->msgoutbuf[i] << 24);
		}
		msgout |= MV_VALID;	/* MV valid */
		msgout |= (unsigned int)data->msgoutlen; /* len */
		nsp32_write4(base, SCSI_MSG_OUT, msgout);
	} else {
		/* data->msgoutlen > 3 */
		nsp32_write4(base, SCSI_MSG_OUT, 0);
	}

	/*
	 * set selection timeout(= 250ms)
	 */
	nsp32_write2(base, SEL_TIME_OUT, SEL_TIMEOUT_TIME);

	/*
	 * set smpl rate
	 * 
	 * TODO: smpl_rate (BASE+0F) is 0 when internal clock = 40MHz.
	 *      check other internal clock!
	 */
	nsp32_write1(base, SREQ_SMPL_RATE, 0);

	/*
	 * clear Arbit
	 */
	nsp32_write1(base, SET_ARBIT, ARBIT_CLEAR);

	/*
	 * set SYNCREG
	 * Don't set BM_START_ADR before setting this register.
	 */
	nsp32_write1(base, SYNC_REG, data->curtarget->syncreg);

	/*
	 * set ACKWIDTH
	 */
	nsp32_write1(base, ACK_WIDTH, data->curtarget->ackwidth);

	nsp32_dbg(NSP32_DEBUG_AUTOSCSI,
		  "syncreg=0x%x, ackwidth=0x%x, sgtpaddr=0x%x, id=0x%x",
		  nsp32_read1(base, SYNC_REG), nsp32_read1(base, ACK_WIDTH),
		  nsp32_read4(base, SGT_ADR), nsp32_read1(base, SCSI_OUT_LATCH_TARGET_ID));
	nsp32_dbg(NSP32_DEBUG_AUTOSCSI, "msgoutlen=%d, msgout=0x%x",
		  data->msgoutlen, msgout);

	/*
	 * set SGT ADDR (physical address)
	 */
	nsp32_write4(base, SGT_ADR, data->curlunt->sglun_paddr);

	/*
	 * set TRANSFER CONTROL REG
	 */
	command = 0;
	command |= ( TRANSFER_GO | ALL_COUNTER_CLR);
	if (data->trans_method & NSP32_TRANSFER_BUSMASTER) {
		if (SCpnt->request_bufflen > 0) {
			command |= BM_START;
		}
	} else if (data->trans_method & NSP32_TRANSFER_MMIO) {
		command |= CB_MMIO_MODE;
	} else if (data->trans_method & NSP32_TRANSFER_PIO) {
		command |= CB_IO_MODE;
	}
	nsp32_write2(base, TRANSFER_CONTROL, command);

	/*
	 * start AUTO SCSI, kick off arbitration
	 */
	command = 0;
	command |= (CLEAR_CDB_FIFO_POINTER
		    | AUTOSCSI_START
		    | AUTO_MSGIN_00_OR_04
		    | AUTO_MSGIN_02 
		    | AUTO_ATN);
	nsp32_write2(base, COMMAND_CONTROL, command);

	/*
	 * Arbitration Status Check
	 *	
	 * Note: Arbitration counter is wait during ARBIT_GO is not lifting.
	 *	 Using udelay(1) consumes CPU time and system time, but 
	 *	 arbitration delay time is defined minimal 2.4us in SCSI
	 *	 specification, thus udelay works as coarse grained wait timer.
	 */
	time = 0;
	while(1) {
		arbit = nsp32_read1(base, ARBIT_STATUS);
		if(arbit & ARBIT_GO) {
			udelay(1);
			time++;
			if ( time > ARBIT_TIMEOUT_TIME ) {
				/* something lock up! guess no connection */
				SCpnt->result = DID_NO_CONNECT << 16;
				status = FALSE;
				goto out;
			}
		} else {
			break;
		}
	};

	nsp32_dbg(NSP32_DEBUG_AUTOSCSI, "arbit: 0x%x, delay time: %d", arbit, time);

	/*
	 * check Arbitration Status Result
	 */
	if(arbit & ARBIT_WIN) {
		/* Arbitration succeeded */
		status = TRUE;
		SCpnt->result = DID_OK << 16;
		/* PCI LED on! */
		nsp32_index_write1(base, EXT_PORT, LED_ON);
	} else if(arbit & ARBIT_FAIL) {
		/* Arbitration failed */
		status = FALSE;
		SCpnt->result = DID_BUS_BUSY << 16;
	} else {
		/* unknown error? */
		status = FALSE;
		SCpnt->result = DID_ERROR << 16;
		SCpnt->result = DID_NO_CONNECT << 16;
	}

	/*
	 * clear Arbit
	 */
	nsp32_write1(base, SET_ARBIT, ARBIT_CLEAR);

 out:
	/*
	 * IRQ enable
	 */
	nsp32_write2(base, IRQ_CONTROL, 0);

	return(status);
}


/*
 * reselection
 *
 * Note: This reselection routine is called from msgin_occur,
 *	 reselection target id&lun must be already set.
 *	 SCSI-2 says IDENTIFY implies RESTORE_POINTER operation.
 */
static int nsp32_reselection(nsp32_hw_data *data, unsigned char newlun)
{
	unsigned int base = data->BaseAddress;
	unsigned char tmpid, newid;

	nsp32_dbg(NSP32_DEBUG_RESELECTION, "enter");

	/*
	 * calculate reselected SCSI ID
	 */
	tmpid = nsp32_read1(base, RESELECT_ID);
	tmpid &= 0x7f;
	newid = 0;
	while (tmpid) {
		if (tmpid & 1) {
			break;
		}
		tmpid >>= 1;
		newid++;
	}

	/*
	 * If reselected New ID:LUN is not existed
	 * or current nexus is not existed, unexpected
	 * reselection is occurred. Send reject message.
	 */
	if (newid >= MAX_TARGET || newlun >= MAX_LUN) {
		nsp32_msg(KERN_WARNING, "unknown id/lun");
		return FALSE;
	} else if(data->lunt[newid][newlun]->SCpnt == NULL) {
		nsp32_msg(KERN_WARNING, "no SCSI command is processing");
		return FALSE;
	}

	data->pid       = newid;
	data->plun      = newlun;
	data->curtarget = &data->target[newid];
	data->curlunt   = data->lunt[newid][newlun];

	/* reset SACK/SavedACK counter (or ALL clear?) */
	nsp32_write4(base, CLR_COUNTER, CLRCOUNTER_ALLMASK);

	return TRUE;
}


/*
 * nsp32hw_setup_sg_table - build scatter gather list for transfer data
 *			    with bus master.
 *
 * Note: NinjaSCSI-32Bi/UDE bus master can not transfer over 64KB at a time.
 */
static int nsp32hw_setup_sg_table(Scsi_Cmnd *SCpnt, nsp32_hw_data *data)
{
	struct scatterlist *sgl;
	struct nsp32_sgtable *sgt = data->curlunt->sglun->sgt;
	int num, i;

	if (SCpnt->request_bufflen == 0) {
		return TRUE;
	}

	if (sgt == NULL) {
		nsp32_dbg(NSP32_DEBUG_SGLIST, "SGT == null");
		return FALSE;
	}

	if (SCpnt->use_sg) {
		sgl = (struct scatterlist *)SCpnt->request_buffer;
		num = pci_map_sg(data->Pci, sgl, SCpnt->use_sg,
				 scsi_to_pci_dma_dir(SCpnt->sc_data_direction));
		for (i=0; i<num; i++) {
			/*
			 * Build nsp32_sglist, substitute sg dma addresses.
			 */
			sgt[i].addr = cpu_to_le32(sg_dma_address(sgl));
			sgt[i].len  = cpu_to_le32(sg_dma_len(sgl));
			sgl++;

			if (sgt[i].len > 65536) {
				nsp32_msg(KERN_ERR,
					"can't transfer over 64KB at a time");
				return FALSE;
			}
			nsp32_dbg(NSP32_DEBUG_SGLIST,
				  "num 0x%x : addr 0x%lx len 0x%x",
				  i, sgt[i].addr, sgt[i].len);
		}
		sgt[num-1].len |= NSP32_SG_END_SGT; /* set end mark */
	} else {
		SCpnt->SCp.have_data_in	= pci_map_single(data->Pci,
			SCpnt->request_buffer, SCpnt->request_bufflen,
			scsi_to_pci_dma_dir(SCpnt->sc_data_direction));
		sgt[0].addr = cpu_to_le32(SCpnt->SCp.have_data_in);
		sgt[0].len  = cpu_to_le32(SCpnt->request_bufflen);
		sgt[0].len |= NSP32_SG_END_SGT; /* set end mark */

		nsp32_dbg(NSP32_DEBUG_SGLIST, "single : addr 0x%lx len=0x%x",
			  sgt[0].addr, sgt[0].len);
	}

	return TRUE;
}

static int nsp32_queuecommand(Scsi_Cmnd *SCpnt, void (*done)(Scsi_Cmnd *))
{
	nsp32_hw_data *data = (nsp32_hw_data *)SCpnt->device->host->hostdata;
	struct nsp32_target *target;
	struct nsp32_lunt *curlunt;
	int ret;

	nsp32_dbg(NSP32_DEBUG_QUEUECOMMAND,
		  "enter. target: 0x%x LUN: 0x%x cmnd: 0x%x cmndlen: 0x%x "
		  "use_sg: 0x%x reqbuf: 0x%lx reqlen: 0x%x",
		  SCpnt->device->id, SCpnt->device->lun, SCpnt->cmnd[0], SCpnt->cmd_len,
		  SCpnt->use_sg, SCpnt->request_buffer, SCpnt->request_bufflen);

	if (data->CurrentSC != NULL ) {
		nsp32_msg(KERN_ERR, "Currentsc != NULL. Cancel this command request");
		data->CurrentSC = NULL;
		SCpnt->result   = DID_NO_CONNECT << 16;
		done(SCpnt);

		return 1;
	}

	/* check target ID is not same as this initiator ID */
	if (SCpnt->device->id == NSP32_HOST_SCSIID) {
		SCpnt->result = DID_BAD_TARGET << 16;
		done(SCpnt);
		return 1;
	}

	/* check target LUN is allowable value */
	if (SCpnt->device->lun >= MAX_LUN) {
		SCpnt->result = DID_BAD_TARGET << 16;
		done(SCpnt);
		return 1;
	}

	show_command(SCpnt);

	SCpnt->scsi_done     = done;
	data->CurrentSC      = SCpnt;
	SCpnt->SCp.Status    = CHECK_CONDITION;
	SCpnt->SCp.Message   = 0;
	SCpnt->resid         = 0; //SCpnt->request_bufflen;

	SCpnt->SCp.ptr		    = (char *) SCpnt->request_buffer;
	SCpnt->SCp.this_residual    = SCpnt->request_bufflen;
	SCpnt->SCp.buffer	    = NULL;
	SCpnt->SCp.buffers_residual = 0;

	/* initialize data */
	data->msgoutlen		= 0;
	data->msginlen		= 0;
	curlunt			= data->lunt[SCpnt->device->id][SCpnt->device->lun];
	curlunt->SCpnt		= SCpnt;
	curlunt->save_datp	= 0;
	curlunt->msgin03	= FALSE;
	data->curlunt		= curlunt;
	data->pid		= SCpnt->device->id;
	data->plun		= SCpnt->device->lun;

	ret = nsp32hw_setup_sg_table(SCpnt, data);
	if (ret == FALSE) {
		SCpnt->result = DID_ERROR << 16;
		nsp32_scsi_done(data, SCpnt);
	}

	/* Build IDENTIFY */
	nsp32_build_identify(data, SCpnt);

	/* 
	 * If target is the first time to transfer after the reset
	 * (target don't have SDTR_DONE and SDTR_INITIATOR), sync
	 * message SDTR is needed to do synchronous transfer.
	 */
	target = &data->target[SCpnt->device->id];
	data->curtarget = target;

	if (!(target->sync_flag & (SDTR_DONE | SDTR_INITIATOR | SDTR_TARGET))) {
		unsigned char period, offset;

		if (trans_mode != ASYNC_MODE) {
			nsp32_set_max_sync(data, target, &period, &offset);
			nsp32_build_sdtr(data, period, offset);
			target->sync_flag |= SDTR_INITIATOR;
		} else {
			nsp32_set_async(data, target);
			target->sync_flag |= SDTR_DONE;
		}

		nsp32_dbg(NSP32_DEBUG_QUEUECOMMAND,
			  "SDTR: entry: %d start_period: 0x%x offset: 0x%x\n",
			  target->limit_entry, period, offset);
	} else if (target->sync_flag & SDTR_INITIATOR) {
		/*
		 * It was negotiating SDTR with target, sending from the
		 * initiator, but there are no chance to remove this flag.
		 * Set async because we don't get proper negotiation.
		 */
		nsp32_set_async(data, target);
		target->sync_flag &= ~SDTR_INITIATOR;
		target->sync_flag |= SDTR_DONE;

		nsp32_dbg(NSP32_DEBUG_QUEUECOMMAND,
			  "SDTR_INITIATOR: fall back to async");
	} else if (target->sync_flag & SDTR_TARGET) {
		/*
		 * It was negotiating SDTR with target, sending from target,
		 * but there are no chance to remove this flag.  Set async
		 * because we don't get proper negotiation.
		 */
		nsp32_set_async(data, target);
		target->sync_flag &= ~SDTR_TARGET;
		target->sync_flag |= SDTR_DONE;

		nsp32_dbg(NSP32_DEBUG_QUEUECOMMAND,
			  "Unknown SDTR from target is reached, fall back to async.");
	}

	nsp32_dbg(NSP32_DEBUG_TARGETFLAG,
		  "target: %d sync_flag: 0x%x syncreg: 0x%x ackwidth: 0x%x",
		  SCpnt->device->id, target->sync_flag, target->syncreg,
		  target->ackwidth);

	/* Selection */
	if (auto_param == 0) {
		ret = nsp32hw_start_selection(SCpnt, data);
	} else {
		ret = nsp32_selection_autoscsi(SCpnt, data);
	}

	if (ret != TRUE) {
		nsp32_scsi_done(data, SCpnt);
		return 1;
	}

	return 0;
}

/* initialize asic */
static int nsp32hw_init(struct Scsi_Host *host)
{
	unsigned int  base = host->io_port;
	unsigned short irq_stat;
	unsigned long lc_reg;
	unsigned char power;
	nsp32_hw_data *data = (nsp32_hw_data *)host->hostdata;

	lc_reg = nsp32_index_read4(base, CFG_LATE_CACHE);
	if ((lc_reg & 0xff00) == 0) {
		lc_reg |= (0x20 << 8);
		nsp32_index_write2(base, CFG_LATE_CACHE, lc_reg & 0xffff);
	}

	nsp32_write2(base, IRQ_CONTROL,      IRQ_CONTROL_ALL_IRQ_MASK);
	nsp32_write2(base, TRANSFER_CONTROL, 0);
	nsp32_write4(base, BM_CNT,           0);
	nsp32_write2(base, SCSI_EXECUTE_PHASE, 0);

	do {
		irq_stat = nsp32_read2(base, IRQ_STATUS);
	} while (irq_stat & IRQSTATUS_ANY_IRQ);
	nsp32_dbg(NSP32_DEBUG_INIT, "irq_stat 0x%x", irq_stat);

	/*
	 * Fill FIFO_FULL_SHLD, FIFO_EMPTY_SHLD. Below parameter is
	 *  designated by specification.
	 */
	if ((data->trans_method & NSP32_TRANSFER_PIO) ||
	    (data->trans_method & NSP32_TRANSFER_MMIO)) {
		nsp32_index_write1(base, FIFO_FULL_SHLD_COUNT, 0x40);
	} else if (data->trans_method & NSP32_TRANSFER_BUSMASTER) {
		nsp32_index_write1(base, FIFO_FULL_SHLD_COUNT, 0x10);
	}
	nsp32_index_write1(base, FIFO_EMPTY_SHLD_COUNT, 0x60);

	nsp32_dbg(NSP32_DEBUG_INIT, "full 0x%x emp 0x%x",
		  nsp32_index_read1(base, FIFO_FULL_SHLD_COUNT),
		  nsp32_index_read1(base, FIFO_EMPTY_SHLD_COUNT));

	nsp32_index_write1(base, CLOCK_DIV, data->clock);
	nsp32_index_write1(base, BM_CYCLE, MEMRD_CMD1 | SGT_AUTO_PARA_MEMED_CMD);
	nsp32_write1(base, PARITY_CONTROL, 0);	/* parity check is disable */

	/*
	 * initialize I_MISC_WRRD register
	 * 
	 * Note: Designated parameters is obeyed as following:
	 *	MISC_SCSI_DIRECTION_DETECTOR_SELECT: It must be set.
	 *	MISC_MASTER_TERMINATION_SELECT:      It must be set.
	 *	MISC_BMREQ_NEGATE_TIMING_SEL:	     It should be set.
	 *	MISC_AUTOSEL_TIMING_SEL:	     It should be set.
	 *	MISC_BMSTOP_CHANGE2_NONDATA_PHASE:   It should be set.
	 *	MISC_DELAYED_BMSTART:		     It's selected for safety.
	 *
	 * Note: If MISC_BMSTOP_CHANGE2_NONDATA_PHASE is set, then
	 *	we have to set TRANSFERCONTROL_BM_START as 0 and set
	 *	appropriate value before restarting bus master transfer.
	 */
	nsp32_index_write2(base, MISC_WR,
			   (SCSI_DIRECTION_DETECTOR_SELECT |
			    DELAYED_BMSTART |
			    MASTER_TERMINATION_SELECT |
			    BMREQ_NEGATE_TIMING_SEL |
			    AUTOSEL_TIMING_SEL |
			    BMSTOP_CHANGE2_NONDATA_PHASE));

	nsp32_index_write1(base, TERM_PWR_CONTROL, 0);
	power = nsp32_index_read1(base, TERM_PWR_CONTROL);
	if (!(power & SENSE)) {
		nsp32_msg(KERN_INFO, "term power on");
		nsp32_index_write1(base, TERM_PWR_CONTROL, BPWR);
	}

	nsp32_write2(base, TIMER_SET, TIMER_STOP);
	nsp32_write2(base, TIMER_SET, TIMER_STOP);

	nsp32_write1(base, SYNC_REG,  0);
	nsp32_write1(base, ACK_WIDTH, 0);
	nsp32_write2(base, SEL_TIME_OUT, 10000); /* 25us x10000 = 250ms defined in SCSI */

	/*
	 * enable to select designated IRQ (except for
	 * IRQSELECT_SERR, IRQSELECT_PERR, IRQSELECT_BMCNTERR)
	 */
	nsp32_index_write2(base, IRQ_SELECT, IRQSELECT_TIMER_IRQ         |
			                     IRQSELECT_SCSIRESET_IRQ     |
			                     IRQSELECT_FIFO_SHLD_IRQ     |
			                     IRQSELECT_RESELECT_IRQ      |
			                     IRQSELECT_PHASE_CHANGE_IRQ  |
			                     IRQSELECT_AUTO_SCSI_SEQ_IRQ |
			                     //IRQSELECT_BMCNTERR_IRQ    |
			                     IRQSELECT_TARGET_ABORT_IRQ  |
			                     IRQSELECT_MASTER_ABORT_IRQ );
	nsp32_write2(base, IRQ_CONTROL, 0);

	/* PCI LED off */
	nsp32_index_write1(base, EXT_PORT_DDR, LED_OFF);
	nsp32_index_write1(base, EXT_PORT, LED_OFF);

	return TRUE;
}


/* interrupt routine */
static irqreturn_t do_nsp32_isr(int irq, void *dev_id, struct pt_regs *regs)
{
	nsp32_hw_data *data = dev_id;
	unsigned int base = data->BaseAddress;
	Scsi_Cmnd *SCpnt = data->CurrentSC;
	unsigned short auto_stat, irq_stat, trans_stat;
	unsigned char busmon, busphase;
	unsigned long flags;
	int ret;
	int handled = 0;

#if LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0)
	struct Scsi_Host *host = data->Host;
	spin_lock_irqsave(host->host_lock, flags);
#else
	spin_lock_irqsave(&io_request_lock, flags);
#endif

	/*
	 * IRQ check, then enable IRQ mask
	 */
	irq_stat = nsp32_read2(base, IRQ_STATUS);
	nsp32_dbg(NSP32_DEBUG_INTR, 
		  "enter IRQ: %d, IRQstatus: 0x%x", irq, irq_stat);
	/* is this interrupt comes from Ninja asic? */
	if ((irq_stat & IRQSTATUS_ANY_IRQ) == 0) {
		nsp32_msg(KERN_INFO, "spurious interrupt: irq other 0x%x", irq_stat);
		goto out2;
	}
	handled = 1;
	nsp32_write2(base, IRQ_CONTROL, IRQ_CONTROL_ALL_IRQ_MASK);

	busmon = nsp32_read1(base, SCSI_BUS_MONITOR);
	busphase = busmon & BUSMON_PHASE_MASK;

	trans_stat = nsp32_read2(base, TRANSFER_STATUS);
	if ((irq_stat == 0xffff) && (trans_stat == 0xffff)) {
		nsp32_msg(KERN_INFO, "card disconnect");
		if (data->CurrentSC != NULL) {
			nsp32_msg(KERN_INFO, "clean up current SCSI command");
			SCpnt->result = DID_BAD_TARGET << 16;
			nsp32_scsi_done(data, SCpnt);
		}
		goto out;
	}

	/* Timer IRQ */
	if (irq_stat & IRQSTATUS_TIMER_IRQ) {
		DEBUG(0, "timer stop\n");
		nsp32_write2(base, TIMER_SET, TIMER_STOP);
		goto out;
	}

	/* SCSI reset */
	if (irq_stat & IRQSTATUS_SCSIRESET_IRQ) {
		nsp32_msg(KERN_INFO, "detected someone do bus reset");
		nsp32_do_bus_reset(data);
		if (SCpnt != NULL) {
			SCpnt->result = DID_RESET << 16;
			nsp32_scsi_done(data, SCpnt);
		}
		goto out;
	}
	
	if (SCpnt == NULL) {
		nsp32_msg(KERN_WARNING, "SCpnt==NULL this can't be happen\n");
		nsp32_msg(KERN_WARNING, "irq_stat=0x%x trans_stat=0x%x\n", irq_stat, trans_stat);
		goto out;
	}

	/*
	 * AutoSCSI Interrupt.
	 * Note: This interrupt is occurred when AutoSCSI is finished.  Then
	 * check SCSIEXECUTEPHASE, and do appropriate action.  Each phases are
	 * recorded when AutoSCSI sequencer has been processed.
	 */
	if(irq_stat & IRQSTATUS_AUTOSCSI_IRQ) {
		/* getting SCSI executed phase */
		auto_stat = nsp32_read2(base, SCSI_EXECUTE_PHASE);
		nsp32_write2(base, SCSI_EXECUTE_PHASE, 0);

		/* Selection Timeout, go busfree phase. */
		if (auto_stat & SELECTION_TIMEOUT) {
			nsp32_dbg(NSP32_DEBUG_INTR,
				  "selection timeout occurred");

			SCpnt->result = DID_TIME_OUT << 16;
			nsp32_scsi_done(data, SCpnt);
			goto out;
		}

		if (auto_stat & MSGOUT_PHASE) {
			/*
			 * MsgOut phase was processed.
			 * If MSG_IN_OCCUER is not set, then MsgOut phase is
			 * completed. Thus, msgoutlen must reset.  Otherwise,
			 * nothing to do here. If MSG_OUT_OCCUER is occurred,
			 * then we will encounter the condition and check.
			 */
			if (!(auto_stat & MSG_IN_OCCUER) &&
			     (data->msgoutlen <= 3)) {
				/*
				 * !MSG_IN_OCCUER && msgoutlen <=3
				 *   ---> AutoSCSI with MSGOUTreg is processed.
				 */
				data->msgoutlen = 0;
			};

			nsp32_dbg(NSP32_DEBUG_INTR, "MsgOut phase processed");
		}

		if ((auto_stat & DATA_IN_PHASE) &&
		    (SCpnt->resid > 0) &&
		    ((nsp32_read2(base, FIFO_REST_CNT) & FIFO_REST_MASK) != 0)) {
			printk( "auto+fifo\n");
			//nsp32_pio_read(SCpnt);
		}

		if (auto_stat & (DATA_IN_PHASE | DATA_OUT_PHASE)) {
			/* DATA_IN_PHASE/DATA_OUT_PHASE was processed. */
			nsp32_dbg(NSP32_DEBUG_INTR,
				  "Data in/out phase processed");

			/* read BMCNT, SGT pointer addr */
			nsp32_dbg(NSP32_DEBUG_INTR, "BMCNT=0x%lx", 
				    nsp32_read4(base, BM_CNT));
			nsp32_dbg(NSP32_DEBUG_INTR, "addr=0x%lx", 
				    nsp32_read4(base, SGT_ADR));
			nsp32_dbg(NSP32_DEBUG_INTR, "SACK=0x%lx", 
				    nsp32_read4(base, SACK_CNT));
			nsp32_dbg(NSP32_DEBUG_INTR, "SSACK=0x%lx", 
				    nsp32_read4(base, SAVED_SACK_CNT));
			
		}

		/*
		 * MsgIn Occur
		 */
		if (auto_stat & MSG_IN_OCCUER) {
			nsp32_msgin_occur(data, irq_stat, auto_stat);
		}

		/*
		 * MsgOut Occur
		 */
		if (auto_stat & MSG_OUT_OCCUER) {
			nsp32_msgout_occur(data);
		}

		/*
		 * Bus Free Occur
		 */
		if (auto_stat & BUS_FREE_OCCUER) {
			ret = nsp32_busfree_occur(data, auto_stat);
			if (ret == TRUE) {
				goto out;
			}
		}

		if (auto_stat & STATUS_PHASE) {
			/*
			 * Read CSB and substitute CSB for SCpnt->result
			 * to save status phase stutas byte.
			 * scsi error handler checks host_byte (DID_*:
			 * low level driver to indicate status), then checks 
			 * status_byte (SCSI status byte).
			 */
			SCpnt->result =	(int)nsp32_read1(base, SCSI_CSB_IN);
		}

		if (auto_stat & ILLEGAL_PHASE) {
			/* Illegal phase is detected. SACK is not back. */
			nsp32_msg(KERN_WARNING, 
				  "AUTO SCSI ILLEGAL PHASE OCCUR!!!!");

			/* TODO: currently we don't have any action... bus reset? */

			/*
			 * To send back SACK, assert, wait, and negate.
			 */
			nsp32_sack_assert(data);
			nsp32_wait_req(data, NEGATE);
			nsp32_sack_negate(data);

		}

		if (auto_stat & COMMAND_PHASE) {
			/* nothing to do */
			nsp32_dbg(NSP32_DEBUG_INTR, "Command phase processed");
		}

		if (auto_stat & AUTOSCSI_BUSY) {
			/* AutoSCSI is running */
		}

		show_autophase(auto_stat);
	}

	/* FIFO_SHLD_IRQ */
	if (irq_stat & IRQSTATUS_FIFO_SHLD_IRQ) {
		nsp32_dbg(NSP32_DEBUG_INTR, "FIFO IRQ");

		switch(busphase) {
		case BUSPHASE_DATA_OUT:
			printk( "write\n");

			//nsp32_pio_write(SCpnt);

			break;

		case BUSPHASE_DATA_IN:
			printk( "read\n");

			//nsp32_pio_read(SCpnt);

			break;

		case BUSPHASE_STATUS:
			//DEBUG(0, "fifo/status\n");

			SCpnt->SCp.Status = nsp32_read1(base, SCSI_CSB_IN);

			break;
		default:
			printk("fifo/other phase?\n");
			printk("irq_stat=0x%x trans_stat=0x%x\n", irq_stat, trans_stat);
			show_busphase(busphase);
			break;
		}

		goto out;
	}

	/* Phase Change IRQ */
	if (irq_stat & IRQSTATUS_PHASE_CHANGE_IRQ) {
		nsp32_dbg(NSP32_DEBUG_INTR, "phase change IRQ");

		switch(busphase) {
		case BUSPHASE_MESSAGE_IN:
			nsp32_dbg(NSP32_DEBUG_INTR, "phase chg/msg in");
			nsp32_msgin_occur(data, irq_stat, 0);
			break;
		default:
			nsp32_msg(KERN_WARNING, "phase chg/other phase?");
			nsp32_msg(KERN_WARNING, "irq_stat=0x%x trans_stat=0x%x\n",
				  irq_stat, trans_stat);
			show_busphase(busphase);
			break;
		}
		goto out;
	}

	/* PCI_IRQ */
	if (irq_stat & IRQSTATUS_PCI_IRQ) {
		nsp32_dbg(NSP32_DEBUG_INTR, "PCI IRQ occurred");
		/* Do nothing */
	}

	/* BMCNTERR_IRQ */
	if (irq_stat & IRQSTATUS_BMCNTERR_IRQ) {
		nsp32_msg(KERN_ERR, "Received unexpected BMCNTERR IRQ! ");
		/*
		 * TODO: To be implemented improving bus master
		 * transfer reliablity when BMCNTERR is occurred in
		 * AutoSCSI phase described in specification.
		 */
	}

#if 0
	printk("irq_stat=0x%x trans_stat=0x%x\n", irq_stat, trans_stat);
	show_busphase(busphase);
#endif

 out:
	/* disable IRQ mask */
	nsp32_write2(base, IRQ_CONTROL, 0);

 out2:
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,5,0)
	spin_unlock_irqrestore(&io_request_lock, flags);
#else
	spin_unlock_irqrestore(host->host_lock, flags);
#endif

	nsp32_dbg(NSP32_DEBUG_INTR, "exit");

	return IRQ_RETVAL(handled);
}

#undef SPRINTF
#define SPRINTF(args...) \
        do { if(pos < buffer + length) pos += sprintf(pos, ## args); } while(0)
static int nsp32_proc_info(char  *buffer,
			   char **start,
			   off_t  offset,
			   int    length,
			   int    hostno,
			   int    inout)
{
	char *pos = buffer;
	int thislength;
	unsigned long flags;
	nsp32_hw_data *data;
	struct Scsi_Host *host = NULL;
	unsigned int base;
	unsigned char mode_reg;

	/* Write is not supported, just return. */
	if (inout == TRUE) {
		return -EINVAL;
	}

	/* search this HBA host */
	
	host = scsi_host_hn_get(hostno);
	
	if (host == NULL) {
		return -ESRCH;
	}
	data = (nsp32_hw_data *)host->hostdata;
	base = host->io_port;

	SPRINTF("NinjaSCSI-32 status\n\n");
	SPRINTF("Driver version:        %s\n",		nsp32_release_version);
	SPRINTF("SCSI host No.:         %d\n",		hostno);
	SPRINTF("IRQ:                   %d\n",		host->irq);
	SPRINTF("IO:                    0x%lx-0x%lx\n", host->io_port, host->io_port + host->n_io_port - 1);
	SPRINTF("MMIO(virtual address): 0x%lx\n",	host->base);
	SPRINTF("sg_tablesize:          %d\n",		host->sg_tablesize);
	SPRINTF("Chip revision:         %d\n",		(nsp32_read2(base, INDEX_REG) >> 8) - 0x4f);

	mode_reg = nsp32_index_read1(base, CHIP_MODE);

#ifdef CONFIG_PM
	//SPRINTF("Power Management:      %s\n",          (mode_reg & OPTF) ? "yes" : "no");
#endif
	SPRINTF("OEM:                   %s\n",          nsp32_model[mode_reg & (OEM0|OEM1)]);

	spin_lock_irqsave(&(data->Lock), flags);
	SPRINTF("CurrentSC:             0x%p\n\n",      data->CurrentSC);
	spin_unlock_irqrestore(&(data->Lock), flags);

	thislength = pos - (buffer + offset);

	if(thislength < 0) {
		*start = 0;
                return 0;
        }


	thislength = MIN(thislength, length);
	*start = buffer + offset;

	return thislength;
}
#undef SPRINTF

/*
 * Note: n_io_port is defined as 0x7f because I/O register port is
 *	 assigned as:
 *	0x800-0x8ff: memory mapped I/O port
 *	0x900-0xbff: (map same 0x800-0x8ff I/O port image repeatedly)
 *	0xc00-0xfff: CardBus status registers
 */
static int nsp32_detect(Scsi_Host_Template *sht)
{
	struct Scsi_Host *host;	/* registered host structure */
	int ret;
	nsp32_hw_data *data;
	int i, j;

	nsp32_dbg(NSP32_DEBUG_REGISTER, "enter");

	/*
	 * register this HBA as SCSI device
	 */
	host = scsi_register(sht, sizeof(nsp32_hw_data));
	if (host == NULL) {
		nsp32_msg (KERN_ERR, "failed to scsi register");
		goto err;
	}

	/*
	 * set nsp32_hw_data
	 */
	data = (nsp32_hw_data *)host->hostdata;
	memset(data, 0, sizeof(nsp32_hw_data));

	data->IrqNumber   = nsp32_data_base.IrqNumber;
	data->BaseAddress = nsp32_data_base.BaseAddress;
	data->NumAddress  = nsp32_data_base.NumAddress; 
	data->MmioAddress = nsp32_data_base.MmioAddress;
	data->Pci         = nsp32_data_base.Pci;
	data->pci_devid   = nsp32_data_base.pci_devid;

	host->irq         = data->IrqNumber;
	host->io_port	  = data->BaseAddress;
	host->unique_id	  = data->BaseAddress;
	host->n_io_port	  = data->NumAddress;
	host->base        = data->MmioAddress;
	scsi_set_device(host, &data->Pci->dev);

	data->Host        = host;
	spin_lock_init(&(data->Lock));

	data->curlunt     = NULL;
	data->curtarget   = NULL;

	/*
	 * Bus master transfer mode is supported currently.
	 */
	data->trans_method	= NSP32_TRANSFER_BUSMASTER;

	/*
	 * Set clock div, CLOCK_4 (HBA has external clock, and
	 * dividing * 100ns/4).
	 * Currently CLOCK_4 has only tested, not for CLOCK_2/PCICLK yet.
	 */
	data->clock = CLOCK_4;

	/*
	 * Select appropriate nsp32_sync_table and set I_CLOCKDIV.
	 */
	switch (data->clock) {
	case CLOCK_4:
		/* If data->clock is CLOCK_4, then select 40M sync table. */
		data->synct = nsp32_sync_table_40M;
		data->syncnum = nsp32_table_40M_num;
		break;
	case CLOCK_2:
		/* If data->clock is CLOCK_2, then select 20M sync table. */
		data->synct = nsp32_sync_table_20M;
		data->syncnum = nsp32_table_20M_num;
		break;
	case PCICLK:
		/* If data->clock is PCICLK, then select pci sync table. */
		data->synct = nsp32_sync_table_pci;
		data->syncnum = nsp32_table_pci_num;
		break;
	default:
		nsp32_msg(KERN_WARNING,
			  "Invalid clock div is selected, set CLOCK_4.");
		/* Use default value CLOCK_4 */
		data->clock = CLOCK_4;
		data->synct = nsp32_sync_table_40M;
		data->syncnum = nsp32_table_40M_num;
	}

	/*
	 * setup nsp32_lunt
	 */
	data->lunt_list = (struct nsp32_lunt *)
		kmalloc(sizeof(struct nsp32_lunt) * MAX_TARGET * MAX_LUN,
			GFP_KERNEL);
	if (data->lunt_list == NULL) {
		nsp32_msg(KERN_ERR, "cannot allocate LUN memory");
		goto scsi_unregister;
	}
	nsp32_dbg(NSP32_DEBUG_REGISTER, "0x%x 0x%x",
		  data->lunt_list, sizeof(struct nsp32_lunt)*MAX_TARGET*MAX_LUN);

	/*
	 * setup DMA 
	 */
	if (pci_set_dma_mask(data->Pci, 0xffffffffUL)) {
		nsp32_msg (KERN_ERR, "failed to set PCI DMA mask");
		goto kfree_lunt;
	}

	/*
	 * allocate autoparam DMA resource.
	 */
	data->autoparam = pci_alloc_consistent(data->Pci, AUTOPARAM_SIZE, &data->apaddr);
	if (data->autoparam == NULL) {
		nsp32_msg(KERN_ERR, "failed to allocate DMA memory");
		goto kfree_lunt;
	}

	/*
	 * allocate scatter-gather DMA resource.
	 */
	data->sg_list = pci_alloc_consistent(data->Pci, 
			(sizeof(struct nsp32_sgtable) * NSP_SG_SIZE * MAX_TARGET * MAX_LUN),
			 &data->sgaddr);
	if (data->sg_list == NULL) {
		nsp32_msg(KERN_ERR, "failed to allocate DMA memory");
		goto free_autoparam;
	}

	for (i=0; i<MAX_TARGET; i++) {
		for (j=0; j<MAX_LUN; j++) {
			data->lunt[i][j] = data->lunt_list + (i * MAX_LUN + j);
		}
	}

	for (i=0; i<MAX_TARGET; i++) {
		for (j=0; j<MAX_LUN; j++) {
			struct nsp32_lunt *lp = data->lunt[i][j];
			lp->sglun = (struct nsp32_sglun *)
				(data->sg_list + (i * MAX_LUN + j));
			lp->sglun_paddr = data->sgaddr +
				(long)((i * MAX_LUN + j) 
				       * sizeof(struct nsp32_sglun));
			lp->SCpnt = NULL;
			lp->save_datp = 0;
			lp->msgin03 = FALSE;
			lp->sg_num = 0;
			lp->cur_entry = 0;
		}
	}

	/*
	 * setup target
	 */
	for (i=0; i<MAX_TARGET; i++) {
		struct nsp32_target *target = &data->target[i];

		target->limit_entry  = 0;
		target->sync_flag    = 0;
		nsp32_set_async(data, target);
	}

	/*
	 * EEPROM check
	 */
	ret = nsp32_getprom_param(data);
	if (ret == FALSE) {
		data->resettime = 3;	/* default 3 */
	}

	/*
	 * setup HBA
	 */
	nsp32hw_init(host);

	snprintf(data->info_str, sizeof(data->info_str),
		 "NinjaSCSI-32Bi/UDE: irq %d, io 0x%lx+0x%x",
		 host->irq, host->io_port, host->n_io_port);

	sht->name = data->info_str;

	/*
	 * SCSI bus reset
	 *
	 * Note: It's important to reset SCSI bus in initialization phase.
	 *     NinjaSCSI-32Bi/UDE HBA EEPROM seems to exchange SDTR when system is
	 *     coming up, so SCSI devices connected to HBA is set as
	 *     un-asynchronous mode.  It brings the merit that this HBA is
	 *     ready to start synchronous transfer without any preparation,
	 *     but we are difficult to control transfer speed.  In addition,
	 *     it prevents device transfer speed from effecting EEPROM start-up
	 *     SDTR.  NinjaSCSI-32Bi/UDE has the feature if EEPROM is set as Auto
	 *     Mode, then FAST-10M is selected when SCSI devices are connected
	 *     same or more than 4 devices.  It should be avoided depending on
	 *     this specification Thus, resetting the SCSI bus restores all
	 *     connected SCSI devices to asynchronous mode, then this driver
	 *     put SDTR safely later, and we can control all SCSI device
	 *     transfer mode.
	 */
	nsp32_do_bus_reset(data);

	ret = request_irq(host->irq, do_nsp32_isr, SA_SHIRQ, "nsp32", data);
	if (ret < 0) {
		nsp32_msg(KERN_ERR, "Unable to allocate IRQ for NSP32 "
			  "SCSI PCI controller. Interrupt: %d\n", host->irq);
		goto free_sg_list;
	}

        /*
         * PCI IO register
         */
	if(!request_region(host->io_port, host->n_io_port, "nsp32")) {
		nsp32_msg(KERN_ERR, 
			  "I/O region 0x%lx+0x%lx is already used",
			  data->BaseAddress, data->length);
		goto free_irq;
        }

	return 1;

 free_irq:
	free_irq(host->irq, data);

 free_autoparam:
	pci_free_consistent(data->Pci, AUTOPARAM_SIZE, data->autoparam, data->apaddr);
	
 free_sg_list:
	pci_free_consistent(data->Pci,
		(sizeof(struct nsp32_sgtable) * NSP_SG_SIZE * MAX_TARGET * MAX_LUN),
		data->sg_list, data->sgaddr);
	
 kfree_lunt:
	kfree(data->lunt_list);

 scsi_unregister:
	scsi_unregister(host);

 err:
	return 0;
}

static int nsp32_release(struct Scsi_Host *shpnt)
{
	nsp32_hw_data *data = (nsp32_hw_data *)shpnt->hostdata;

	if (data->lunt_list) {
		kfree(data->lunt_list);
	}

	if (data->autoparam) {
		pci_free_consistent(data->Pci, AUTOPARAM_SIZE,
					data->autoparam, data->apaddr);
	}

	if (data->sg_list) {
		pci_free_consistent(data->Pci, 
			(sizeof(struct nsp32_sgtable) * NSP_SG_SIZE * MAX_TARGET * MAX_LUN),
			data->sg_list, data->sgaddr);
	}

	DEBUG(0, "free irq\n");
	if (shpnt->irq) {
		free_irq(shpnt->irq, data);
	}

	DEBUG(0, "free io\n");
	if (shpnt->io_port && shpnt->n_io_port) {
		release_region(shpnt->io_port, shpnt->n_io_port);
	}

	if (data->MmioAddress != 0) {
		iounmap((void *)(data->MmioAddress));
	}

	return 0;
}

static const char *nsp32_info(struct Scsi_Host *shpnt)
{
	nsp32_hw_data *data = (nsp32_hw_data *)shpnt->hostdata;

	return data->info_str;
}


/*
 * error handler
 */
static int nsp32_reset(Scsi_Cmnd *SCpnt, unsigned int reset_flags)
{
	nsp32_dbg(NSP32_DEBUG_BUSRESET, "SCpnt=0x%p why=%d\n", SCpnt, reset_flags);

	nsp32_eh_bus_reset(SCpnt);

	return SCSI_RESET_SUCCESS | SCSI_RESET_BUS_RESET;
}

static int nsp32_eh_abort(Scsi_Cmnd *SCpnt)
{
	nsp32_hw_data *data = (nsp32_hw_data *)SCpnt->device->host->hostdata;
	unsigned int base = data->BaseAddress;

	nsp32_msg(KERN_WARNING, "abort");

	if (data->curlunt->SCpnt == NULL) {
		return (FAILED);
	}

	if (data->curtarget->sync_flag & (SDTR_INITIATOR | SDTR_TARGET)) {
		/* reset SDTR negotiation */
		data->curtarget->sync_flag = 0;
	}

	nsp32_write2(base, TRANSFER_CONTROL, 0);
	nsp32_write2(base, BM_CNT, 0);

	return (FAILED);
}

static int nsp32_eh_bus_reset(Scsi_Cmnd *SCpnt)
{
	nsp32_hw_data *data = (nsp32_hw_data *)SCpnt->device->host->hostdata;
	unsigned int base = data->BaseAddress;

	nsp32_msg(KERN_INFO, "Bus Reset");	
	nsp32_dbg(NSP32_DEBUG_BUSRESET, "SCpnt=0x%x", SCpnt);

	nsp32_write2(base, IRQ_CONTROL, IRQ_CONTROL_ALL_IRQ_MASK);
	nsp32_do_bus_reset(data);
	nsp32_write2(base, IRQ_CONTROL, 0);

	return SUCCESS;	/* SCSI bus reset is succeeded at any time. */
}

static void nsp32_do_bus_reset(nsp32_hw_data *data)
{
	unsigned int base = data->BaseAddress;
	unsigned short intrdat;
	int i;

	/*
	 * stop all transfer
	 * clear TRANSFERCONTROL_BM_START
	 * clear counter
	 */
	nsp32_write2(base, TRANSFER_CONTROL, 0);
	nsp32_write4(base, BM_CNT,           0);
	nsp32_write4(base, CLR_COUNTER, CLRCOUNTER_ALLMASK);

	/*
	 * fall back to asynchronous transfer mode
	 * initialize SDTR negotiation flag
	 */
	for (i=0; i<MAX_TARGET; i++) {
		struct nsp32_target *target = &data->target[i];

		target->sync_flag = 0;
		nsp32_set_async(data, target);
	}

	/*
	 * reset SCSI bus
	 */
	nsp32_write1(base, SCSI_BUS_CONTROL, BUSCTL_RST);
	udelay(RESET_HOLD_TIME);
	nsp32_write1(base, SCSI_BUS_CONTROL, 0);
	for(i = 0; i < 5; i++) {
		intrdat = nsp32_read2(base, IRQ_STATUS); /* dummy read */
		nsp32_dbg(NSP32_DEBUG_BUSRESET, "irq:1: 0x%x", intrdat);
        }

	data->CurrentSC = NULL;
}

static int nsp32_eh_host_reset(Scsi_Cmnd *SCpnt)
{
	struct Scsi_Host *host = SCpnt->device->host;
	nsp32_hw_data *data = (nsp32_hw_data *)host->hostdata;
	unsigned int base = data->BaseAddress;

	nsp32_msg(KERN_INFO, "Host Reset");	
	nsp32_dbg(NSP32_DEBUG_BUSRESET, "SCpnt=0x%x", SCpnt);

	nsp32hw_init(host);
	nsp32_write2(base, IRQ_CONTROL, IRQ_CONTROL_ALL_IRQ_MASK);
	nsp32_do_bus_reset(data);
	nsp32_write2(base, IRQ_CONTROL, 0);

	return SUCCESS;	/* Host reset is succeeded at any time. */
}

/*
 * PCI/Cardbus probe/remove routine
 */
static int __devinit nsp32_probe(struct pci_dev *pdev, const struct pci_device_id *id)
{
	int ret;
	nsp32_hw_data *data = &nsp32_data_base;

	nsp32_dbg(NSP32_DEBUG_REGISTER, "enter");

        ret = pci_enable_device(pdev);
	if (ret) {
		nsp32_msg(KERN_ERR, "failed to enable pci device");
		return ret;
	}

	data->Pci = pdev;
	data->pci_devid = id;
	data->IrqNumber = pdev->irq;
	data->BaseAddress = pci_resource_start(pdev, 0);
	data->NumAddress = pci_resource_len(pdev, 0);
	data->MmioAddress = (unsigned long)ioremap_nocache(
		pci_resource_start(pdev, 1), pci_resource_len(pdev, 1));

	pci_set_master(pdev);

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,2))
	scsi_register_host(&driver_template);
#else
	scsi_register_module(MODULE_SCSI_HA, &driver_template);
#endif

	nsp32_msg(KERN_INFO, "nsp32 irq: %i mmio: 0x%lx slot: %s model: %s",
		  pdev->irq, data->MmioAddress, pdev->slot_name,
		  nsp32_model[id->driver_data]);

	nsp32_dbg(NSP32_DEBUG_REGISTER, "exit");

	return 0;
}

static void __devexit nsp32_remove(struct pci_dev *pdev)
{
	nsp32_dbg(NSP32_DEBUG_REGISTER, "enter");
	
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,2))
	scsi_unregister_host(&driver_template);
#else
	scsi_unregister_module(MODULE_SCSI_HA, &driver_template);
#endif
}

static struct pci_device_id nsp32_pci_table[] __devinitdata = {
	{
		.vendor      = PCI_VENDOR_ID_IODATA,
		.device      = PCI_DEVICE_ID_NINJASCSI_32BI_CBSC_II,
		.subvendor   = PCI_ANY_ID,
		.subdevice   = PCI_ANY_ID,
		.driver_data = MODEL_IODATA,
	},
	{
		.vendor      = PCI_VENDOR_ID_WORKBIT,
		.device      = PCI_DEVICE_ID_NINJASCSI_32BI_KME,
		.subvendor   = PCI_ANY_ID,
		.subdevice   = PCI_ANY_ID,
		.driver_data = MODEL_KME,
	},
	{
		.vendor      = PCI_VENDOR_ID_WORKBIT,
		.device      = PCI_DEVICE_ID_NINJASCSI_32BI_WBT,
		.subvendor   = PCI_ANY_ID,
		.subdevice   = PCI_ANY_ID,
		.driver_data = MODEL_WORKBIT,
	},
	{
		.vendor      = PCI_VENDOR_ID_WORKBIT,
		.device      = PCI_DEVICE_ID_WORKBIT_STANDARD,
		.subvendor   = PCI_ANY_ID,
		.subdevice   = PCI_ANY_ID,
		.driver_data = MODEL_PCI_WORKBIT,
	},
	{
		.vendor      = PCI_VENDOR_ID_WORKBIT,
		.device      = PCI_DEVICE_ID_NINJASCSI_32BI_LOGITEC,
		.subvendor   = PCI_ANY_ID,
		.subdevice   = PCI_ANY_ID,
		.driver_data = MODEL_EXT_ROM,
	},
	{
		.vendor      = PCI_VENDOR_ID_WORKBIT,
		.device      = PCI_DEVICE_ID_NINJASCSI_32BIB_LOGITEC,
		.subvendor   = PCI_ANY_ID,
		.subdevice   = PCI_ANY_ID,
		.driver_data = MODEL_PCI_LOGITEC,
	},
	{
		.vendor      = PCI_VENDOR_ID_WORKBIT,
		.device      = PCI_DEVICE_ID_NINJASCSI_32UDE_MELCO,
		.subvendor   = PCI_ANY_ID,
		.subdevice   = PCI_ANY_ID,
		.driver_data = MODEL_PCI_MELCO,
	},
	{0,0,},
};
MODULE_DEVICE_TABLE(pci, nsp32_pci_table);

static struct pci_driver nsp32_driver = {
	.name =		"nsp32",
	.id_table =	nsp32_pci_table,
	.probe =	nsp32_probe,
	.remove =	nsp32_remove,
#ifdef CONFIG_PM
/*	.suspend =	nsp32_suspend,*/
/*	.resume =	nsp32_resume,*/
#endif
};

static int __init init_nsp32(void) {
	return pci_module_init(&nsp32_driver);
}

static void __exit exit_nsp32(void) {
	pci_unregister_driver(&nsp32_driver);
}

module_init(init_nsp32);
module_exit(exit_nsp32);


/*
 * Reset parameters and call scsi_done for data->curlunt.
 * Be careful setting SCpnt->result = DID_* before calling this function.
 */
static void nsp32_scsi_done(nsp32_hw_data *data, Scsi_Cmnd *SCpnt)
{
	unsigned int base = data->BaseAddress;

	/*
	 * unmap pci
	 */
	if (SCpnt->request_bufflen == 0) {
		goto skip;
	}

	if (SCpnt->use_sg) {
		pci_unmap_sg(data->Pci,
			(struct scatterlist *)SCpnt->buffer,
			SCpnt->use_sg,
			scsi_to_pci_dma_dir(SCpnt->sc_data_direction));
	} else {
		pci_unmap_single(data->Pci,
			(u32)SCpnt->SCp.have_data_in,
			SCpnt->request_bufflen,
			scsi_to_pci_dma_dir(SCpnt->sc_data_direction));
	}

 skip:
	/*
	 * clear TRANSFERCONTROL_BM_START
	 */
	nsp32_write2(base, TRANSFER_CONTROL, 0);
	nsp32_write4(base, BM_CNT, 0);

	/*
	 * call scsi_done
	 */
	(*SCpnt->scsi_done)(SCpnt);

	/*
	 * reset parameters
	 */
	data->curlunt->SCpnt = NULL;
	data->curlunt = NULL;
	data->curtarget = NULL;
	data->CurrentSC = NULL;
}


/*
 * Bus Free Occur
 *
 * Current Phase is BUSFREE. AutoSCSI is automatically execute BUSFREE phase
 * with ACK reply when below condition is matched:
 *	MsgIn 00: Command Complete.
 *	MsgIn 02: Save Data Pointer.
 *	MsgIn 04: Diconnect.
 * In other case, unexpected BUSFREE is detected.
 */
static int nsp32_busfree_occur(nsp32_hw_data *data, unsigned short execph)
{
	Scsi_Cmnd *SCpnt = data->curlunt->SCpnt;
	unsigned int base = data->BaseAddress;

	nsp32_dbg(NSP32_DEBUG_BUSFREE, "enter");

	nsp32_write4(base, BM_CNT, 0);
	nsp32_write2(base, TRANSFER_CONTROL, 0);

	/*
	 * MsgIn 02: Save Data Pointer
	 *
	 * VALID:
	 *   Save Data Pointer is received. Adjust pointer.
	 *   
	 * NO-VALID:
	 *   SCSI-3 says if Save Data Pointer is not received, then we restart
	 *   processing and we can't adjust any SCSI data pointer in next data
	 *   phase.
	 */
	if (execph & MSGIN_02_VALID) {
		nsp32_dbg(NSP32_DEBUG_BUSFREE, "MsgIn02_Valid");

		/*
		 * Check sack_cnt/saved_sack_cnt, then adjust sg table if
		 * needed.
		 */
		if (!(execph & MSGIN_00_VALID) && 
		    ((execph & DATA_IN_PHASE) || (execph & DATA_OUT_PHASE))) {
			unsigned int sacklen, s_sacklen;

			/*
			 * Read SACK count and SAVEDSACK count, then compare.
			 */
			sacklen   = nsp32_read4(base, SACK_CNT);
			s_sacklen = nsp32_read4(base, SAVED_SACK_CNT);

			/*
			 * If SAVEDSACKCNT == 0, it means SavedDataPointer is
			 * come after data transfering.
			 */
			if (s_sacklen > 0) {
				/*
				 * Comparing between sack and savedsack to
				 * check the condition of AutoMsgIn03.
				 *
				 * If they are same, set msgin03 == TRUE,
				 * COMMANDCONTROL_AUTO_MSGIN_03 is enabled at
				 * reselection.  On the other hand, if they
				 * aren't same, set msgin03 == FALSE, and
				 * COMMANDCONTROL_AUTO_MSGIN_03 is disabled at
				 * reselection.
				 */
				if (sacklen != s_sacklen) {
					data->curlunt->msgin03 = FALSE;
				} else {
					data->curlunt->msgin03 = TRUE;
				}

				nsp32_adjust_busfree(data, s_sacklen);
			}
		}

		/* This value has not substitude with valid value yet... */
		//data->curlunt->save_datp = data->cur_datp;
	} else {
		/*
		 * no processing.
		 */
	}
	
	if (execph & MSGIN_03_VALID) {
		/* MsgIn03 was valid to be processed. No need processing. */
	}

	/*
	 * target SDTR check
	 */
	if (data->curtarget->sync_flag & SDTR_INITIATOR) {
		/*
		 * SDTR negotiation pulled by the initiator has not
		 * finished yet. Fall back to ASYNC mode.
		 */
		nsp32_set_async(data, data->curtarget);
		data->curtarget->sync_flag &= ~SDTR_INITIATOR;
		data->curtarget->sync_flag |= SDTR_DONE;
	} else if (data->curtarget->sync_flag & SDTR_TARGET) {
		/*
		 * SDTR negotiation pulled by the target has been
		 * negotiating.
		 */
		if (execph & (MSGIN_00_VALID | MSGIN_04_VALID)) {
			/* 
			 * If valid message is received, then
			 * negotiation is succeeded.
			 */
		} else {
			/*
			 * On the contrary, if unexpected bus free is
			 * occurred, then negotiation is failed. Fall
			 * back to ASYNC mode.
			 */
			nsp32_set_async(data, data->curtarget);
		}
		data->curtarget->sync_flag &= ~SDTR_TARGET;
		data->curtarget->sync_flag |= SDTR_DONE;
	}
	
	/*
	 * It is always ensured by SCSI standard that initiator
	 * switches into Bus Free Phase after
	 * receiving message 00 (Command Complete), 04 (Disconnect).
	 * It's the reason that processing here is valid.
	 */
	if (execph & MSGIN_00_VALID) {
		/* MsgIn 00: Command Complete */
		nsp32_dbg(NSP32_DEBUG_BUSFREE, "command complete");

		SCpnt->SCp.Status  = nsp32_read1(base, SCSI_CSB_IN);
		SCpnt->SCp.Message = 0;
		nsp32_dbg(NSP32_DEBUG_BUSFREE, 
			  "normal end stat=0x%x resid=0x%x\n",
			  SCpnt->SCp.Status, SCpnt->resid);
		SCpnt->result = 
			(DID_OK << 16) | (SCpnt->SCp.Message << 8) | (SCpnt->SCp.Status << 0);
		nsp32_scsi_done(data, SCpnt);
		/* All operation is done */
		return (TRUE);
	} else if (execph & MSGIN_04_VALID) {
		/* MsgIn 04: Disconnect */
		SCpnt->SCp.Status  = nsp32_read1(base, SCSI_CSB_IN);
		SCpnt->SCp.Message = 4;
		
		nsp32_dbg(NSP32_DEBUG_BUSFREE, "disconnect");
		return (TRUE);
	} else {
		/* Unexpected bus free */
		nsp32_msg(KERN_WARNING, "unexpected bus free occurred");

		/* DID_ERROR? */
		//SCpnt->result   = (DID_OK << 16) | (SCpnt->SCp.Message << 8) | (SCpnt->SCp.Status << 0);
		SCpnt->result = DID_ERROR << 16;
		nsp32_scsi_done(data, SCpnt);
		return (TRUE);
	}
	return (FALSE);
}


/*
 * nsp32_adjust_busfree - adjusting SG table
 *
 * Note: This driver adjust the SG table using SCSI ACK
 *       counter instead of BMCNT counter!
 */
static void nsp32_adjust_busfree(nsp32_hw_data *data, unsigned int s_sacklen)
{
	int old_entry = data->cur_entry;
	int new_entry;
	struct nsp32_sgtable *sgt = data->curlunt->sglun->sgt;
	unsigned int restlen, sentlen;
	int sg_num = data->curlunt->sg_num;

	/* adjust saved SACK count with 4 byte start address boundary */
	s_sacklen -= sgt[old_entry].addr & 3;

	/*
	 * calculate new_entry from sack count and each sgt[].len 
	 * calculate the byte which is intent to send
	 */
	sentlen = 0;
	for (new_entry = old_entry; new_entry < sg_num; new_entry++) {
		sentlen += (sgt[new_entry].len & ~NSP32_SG_END_SGT);
		if (sentlen > s_sacklen) {
			break;
		}
	}

	/* all sgt is processed */
	if (new_entry == sg_num) {
		goto last;
	}

	if (sentlen == s_sacklen) {
		/* XXX: confirm it's ok or not */
		/* In this case, it's ok because we are at 
		   the head element of the sg. restlen is correctly calculated. */
	}

	/* calculate the rest length for transfering */
	restlen = sentlen - s_sacklen;
	
	/* update adjusting current SG table entry */
	sgt[new_entry].addr += (sgt[new_entry].len - restlen);
	sgt[new_entry].len = restlen;

	/* set cur_entry with new_entry */
	data->cur_entry = new_entry;
	
	return;

 last:
	/* update hostdata and lun */

	return;
}


/*
 * It's called MsgOut phase occur.
 * NinjaSCSI-32Bi/UDE automatically processes up to 3 messages in
 * message out phase. It, however, has more than 3 messages,
 * HBA creates the interrupt and we have to process by hand.
 */
static void nsp32_msgout_occur(nsp32_hw_data *data)
{
	unsigned int base = data->BaseAddress;
	long new_sgtp;
	int i;
	
	nsp32_dbg(NSP32_DEBUG_MSGOUTOCCUR,
		  "enter: msgoutlen: 0x%x", data->msgoutlen);

	/*
	 * If MsgOut phase is occurred without having any
	 * message, then No_Operation is sent (SCSI-2).
	 */
	if (data->msgoutlen == 0) {
		nsp32_build_nop(data);
	}

	/*
	 * Set SGTP ADDR current entry for restarting AUTOSCSI, 
	 * because SGTP is incremented next point.
	 * There is few statement in the specification...
	 */
 	new_sgtp = data->curlunt->sglun_paddr
		+ data->curlunt->cur_entry * sizeof(struct nsp32_sgtable);

	/*
	 * send messages
	 */
	for (i=0; i<data->msgoutlen; i++) {
		nsp32_dbg(NSP32_DEBUG_MSGOUTOCCUR,
			  "%d : 0x%x", i, data->msgoutbuf[i]);

		/*
		 * Check REQ is asserted.
		 */
		nsp32_wait_req(data, ASSERT);

		if (i == (data->msgoutlen - 1)) {
			/*
			 * If the last message, set the AutoSCSI restart
			 * before send back the ack message. AutoSCSI
			 * restart automatically negate ATN signal.
			 */
			//command = (AUTO_MSGIN_00_OR_04 | AUTO_MSGIN_02);
			//nsp32_restart_autoscsi(data, command);
			nsp32_write2(base, COMMAND_CONTROL,
					 (CLEAR_CDB_FIFO_POINTER |
					  AUTO_COMMAND_PHASE |
					  AUTOSCSI_RESTART |
					  AUTO_MSGIN_00_OR_04 |
					  AUTO_MSGIN_02 ));
		}
		/*
		 * Write data with SACK, then wait sack is
		 * automatically negated.
		 */
		nsp32_write1(base, SCSI_DATA_WITH_ACK, data->msgoutbuf[i]);
		nsp32_wait_sack(data, NEGATE);

		nsp32_dbg(NSP32_DEBUG_MSGOUTOCCUR, "bus: 0x%x\n",
			  nsp32_read1(base, SCSI_BUS_MONITOR));
	};

	data->msgoutlen = 0;

	nsp32_dbg(NSP32_DEBUG_MSGOUTOCCUR, "exit");
}

/*
 * Restart AutoSCSI
 *
 * Note: Restarting AutoSCSI needs set:
 *		SYNC_REG, ACK_WIDTH, SGT_ADR, TRANSFER_CONTROL
 */
static void nsp32_restart_autoscsi(nsp32_hw_data *data, unsigned short command)
{
	unsigned int base = data->BaseAddress;
	unsigned short transfer = 0;
	Scsi_Cmnd *SCpnt = data->curlunt->SCpnt;

	nsp32_dbg(NSP32_DEBUG_RESTART, "enter");

	if (data->curtarget == NULL || data->curlunt == NULL) {
		nsp32_msg(KERN_ERR, "Target or Lun is invalid");
	}

	/*
	 * set SYNC_REG
	 * Don't set BM_START_ADR before setting this register.
	 */
	nsp32_write1(base, SYNC_REG, data->curtarget->syncreg);

	/*
	 * set ACKWIDTH
	 */
	nsp32_write1(base, ACK_WIDTH, data->curtarget->ackwidth);

	/*
	 * set SGT ADDR (physical address)
	 */
	nsp32_write4(base, SGT_ADR, data->curlunt->sglun_paddr);

	/*
	 * set TRANSFER CONTROL REG
	 */
	transfer = 0;
	transfer |= (TRANSFER_GO | ALL_COUNTER_CLR);
	if (data->trans_method & NSP32_TRANSFER_BUSMASTER) {
		if (SCpnt->request_bufflen > 0) {
			transfer |= BM_START;
		}
	} else if (data->trans_method & NSP32_TRANSFER_MMIO) {
		transfer |= CB_MMIO_MODE;
	} else if (data->trans_method & NSP32_TRANSFER_PIO) {
		transfer |= CB_IO_MODE;
	}
	nsp32_write2(base, TRANSFER_CONTROL, transfer);

	/*
	 * restart AutoSCSI
	 *
	 * TODO: COMMANDCONTROL_AUTO_COMMAND_PHASE is needed ?
	 */
	command |= (CLEAR_CDB_FIFO_POINTER |
		    AUTO_COMMAND_PHASE |
		    AUTOSCSI_RESTART);
	nsp32_write2(base, COMMAND_CONTROL, command);

	nsp32_dbg(NSP32_DEBUG_RESTART, "exit");
}


/*
 * cannot run automatically message in occur
 */
static void nsp32_msgin_occur(nsp32_hw_data *data, unsigned long irq_status,
			unsigned short execph)
{
	unsigned int base = data->BaseAddress;
	unsigned char msg;
	unsigned char msgtype;
	unsigned char newlun;
	unsigned short command = 0;
	int msgclear = TRUE;
	long new_sgtp;
	int ret;

	/*
	 * read first message
	 *    Use SCSIDATA_W_ACK instead of SCSIDATAIN, because the procedure
	 *    of Message-In have to be processed before sending back SCSI ACK.
	 */
	msg = nsp32_read1(base, SCSI_DATA_IN);
	data->msginbuf[(unsigned char)data->msginlen] = msg;
	msgtype = data->msginbuf[0];
	nsp32_dbg(NSP32_DEBUG_MSGINOCCUR,
		  "enter: msglen: 0x%x msgin: 0x%x msgtype: 0x%x",
		  data->msginlen, msg, msgtype);

	/*
	 * TODO: We need checking whether bus phase is message in?
	 */

	/*
	 * assert SCSI ACK
	 */
	nsp32_sack_assert(data);

	/*
	 * processing IDENTIFY
	 */
	if (msgtype & 0x80) {
		if (!(irq_status & IRQSTATUS_RESELECT_OCCUER)) {
			/* Invalid (non reselect) phase */
			goto reject;
		}

		newlun = msgtype & 0x1f; /* TODO: SPI-3 compliant? */
		ret = nsp32_reselection(data, newlun);
		if (ret == TRUE) {
			goto restart;
		} else {
			goto reject;
		}
	}
	
	/*
	 * processing messages except for IDENTIFY
	 *
	 * TODO: Messages are all SCSI-2 terminology. SCSI-3 compliance is TODO.
	 */
	switch (msgtype) {
	/*
	 * 1-byte message
	 */
	case COMMAND_COMPLETE:
	case DISCONNECT:
		/*
		 * These messages should not be occurred.
		 * They should be processed on AutoSCSI sequencer.
		 */
		nsp32_msg(KERN_WARNING, 
			   "unexpected message of AutoSCSI MsgIn: 0x%x", msg);
		break;
		
	case RESTORE_POINTERS:
		/*
		 * AutoMsgIn03 is disabled, and HBA gets this message.
		 */

		if ((execph & DATA_IN_PHASE) || (execph & DATA_OUT_PHASE)) {
			unsigned int s_sacklen;

			s_sacklen = nsp32_read4(base, SAVED_SACK_CNT);
			if ((execph & MSGIN_02_VALID) && (s_sacklen > 0)) {
				nsp32_adjust_busfree(data, s_sacklen);
			} else {
				/* No need to rewrite SGT */
			}
		}
		data->curlunt->msgin03 = FALSE;

		/* Update with the new value */

		/* reset SACK/SavedACK counter (or ALL clear?) */
		nsp32_write4(base, CLR_COUNTER, CLRCOUNTER_ALLMASK);

		/*
		 * set new sg pointer
		 */
		new_sgtp = data->curlunt->sglun_paddr + 
			data->curlunt->cur_entry * sizeof(struct nsp32_sgtable);
		nsp32_write4(base, SGT_ADR, new_sgtp);

		break;

	case SAVE_POINTERS:
		/*
		 * These messages should not be occurred.
		 * They should be processed on AutoSCSI sequencer.
		 */
		nsp32_msg (KERN_WARNING, 
			   "unexpected message of AutoSCSI MsgIn: SAVE_POINTERS");
		
		break;
		
	case MESSAGE_REJECT:
		/* If previous message_out is sending SDTR, and get 
		   message_reject from target, SDTR negotiation is failed */
		if (data->curtarget->sync_flag &
				(SDTR_INITIATOR | SDTR_TARGET)) {
			/*
			 * Current target is negotiating SDTR, but it's
			 * failed.  Fall back to async transfer mode, and set
			 * SDTR_DONE.
			 */
			nsp32_set_async(data, data->curtarget);
			data->curtarget->sync_flag &= ~SDTR_INITIATOR;
			data->curtarget->sync_flag |= SDTR_DONE;

		}
		break;

	case LINKED_CMD_COMPLETE:
	case LINKED_FLG_CMD_COMPLETE:
		/* queue tag is not supported currently */
		nsp32_msg (KERN_WARNING, 
			   "unsupported message: 0x%x", msgtype);
		break;

	case INITIATE_RECOVERY:
		/* staring ECA (Extended Contingent Allegiance) state. */
		/* This message is declined in SPI2 or later. */

		goto reject;

	/*
	 * 2-byte message
	 */
	case SIMPLE_QUEUE_TAG:
	case 0x23:
		/*
		 * 0x23: Ignore_Wide_Residue is not declared in scsi.h.
		 * No support is needed.
		 */
		if (data->msginlen >= 1) {
			goto reject;
		}

		/* current position is 1-byte of 2 byte */
		msgclear = FALSE;

		break;

	/*
	 * extended message
	 */
	case EXTENDED_MESSAGE:
		if (data->msginlen < 1) {
			/*
			 * Current position does not reach 2-byte
			 * (2-byte is extended message length).
			 */
			msgclear = FALSE;
			break;
		}

		if ((data->msginbuf[1] + 1) > data->msginlen) {
			/*
			 * Current extended message has msginbuf[1] + 2
			 * (msginlen starts counting from 0, so buf[1] + 1).
			 * If current message position is not finished,
			 * continue receiving message.
			 */
			msgclear = FALSE;
			break;
		}

		/*
		 * Reach here means regular length of each type of 
		 * extended messages.
		 */
		switch (data->msginbuf[2]) {
		case EXTENDED_MODIFY_DATA_POINTER:
			/* TODO */
			goto reject; /* not implemented yet */
			break;

		case EXTENDED_SDTR:
			/*
			 * Exchange this message between initiator and target.
			 */
			if (data->msginlen != EXTENDED_SDTR_LEN + 1) {
				/*
				 * received inappropriate message.
				 */
				goto reject;
				break;
			}

			nsp32_analyze_sdtr(data);

			break;

		case EXTENDED_EXTENDED_IDENTIFY:
			/* SCSI-I only, not supported. */
			goto reject; /* not implemented yet */

			break;

		case EXTENDED_WDTR:
			goto reject; /* not implemented yet */

			break;
			
		default:
			goto reject;
		}
		break;
		
	default:
		goto reject;
	}

 restart:
	if (msgclear == TRUE) {
		data->msginlen = 0;

		/*
		 * If restarting AutoSCSI, but there are some message to out
		 * (msgoutlen > 0), set AutoATN, and set SCSIMSGOUT as 0
		 * (MV_VALID = 0). When commandcontrol is written with
		 * AutoSCSI restart, at the same time MsgOutOccur should be
		 * happened (however, such situation is really possible...?).
		 */
		if (data->msgoutlen > 0) {	
			nsp32_write4(base, SCSI_MSG_OUT, 0);
			command |= AUTO_ATN;
		}

		/*
		 * restart AutoSCSI
		 * If it's failed, COMMANDCONTROL_AUTO_COMMAND_PHASE is needed.
		 */
		command |= (AUTO_MSGIN_00_OR_04 | AUTO_MSGIN_02);

		/*
		 * If current msgin03 is TRUE, then flag on.
		 */
		if (data->curlunt->msgin03 == TRUE) {
			command |= AUTO_MSGIN_03;
		}
		data->curlunt->msgin03 = FALSE;
	} else {
		data->msginlen++;
	}

	/*
	 * restart AutoSCSI
	 */
	nsp32_restart_autoscsi(data, command);

	/*
	 * wait SCSI REQ negate for REQ-ACK handshake
	 */
	nsp32_wait_req(data, NEGATE);

	/*
	 * negate SCSI ACK
	 */
	nsp32_sack_negate(data);

	nsp32_dbg(NSP32_DEBUG_MSGINOCCUR, "exit");

	return;

 reject:
	nsp32_msg(KERN_WARNING, 
		  "invalid or unsupported MessageIn, rejected. "
		  "current msg: 0x%x (len: 0x%x), processing msg: 0x%x",
		  msg, data->msginlen, msgtype);
	nsp32_build_reject(data);
	data->msginlen = 0;

	goto restart;
}

/*
 * 
 */
static void nsp32_analyze_sdtr(nsp32_hw_data *data)
{
	struct nsp32_target *target = data->curtarget;
	struct nsp32_sync_table *synct;
	unsigned char get_period = data->msginbuf[3];
	unsigned char get_offset = data->msginbuf[4];
	int entry;
	int syncnum;

	nsp32_dbg(NSP32_DEBUG_MSGINOCCUR, "enter");

	synct = data->synct;
	syncnum = data->syncnum;

	/*
	 * If this inititor sent the SDTR message, then target responds SDTR,
	 * initiator SYNCREG, ACKWIDTH from SDTR parameter.
	 * Messages are not appropriate, then send back reject message.
	 * If initiator did not send the SDTR, but target sends SDTR, 
	 * initiator calculator the appropriate parameter and send back SDTR.
	 */	
	if (target->sync_flag & SDTR_INITIATOR) {
		/*
		 * Initiator sent SDTR, the target responds and
		 * send back negotiation SDTR.
		 */
		nsp32_dbg(NSP32_DEBUG_MSGINOCCUR, "target responds SDTR");
	
		target->sync_flag &= ~SDTR_INITIATOR;
		target->sync_flag |= SDTR_DONE;

		/*
		 * offset:
		 */
		if (get_offset > SYNC_OFFSET) {
			/*
			 * Negotiation is failed, the target send back
			 * unexpected offset value.
			 */
			goto reject;
		}
		
		if (get_offset == ASYNC_OFFSET) {
			/*
			 * Negotiation is succeeded, the target want
			 * to fall back into asynchronous transfer mode.
			 */
			goto async;
		}

		/*
		 * period:
		 *    Check whether sync period is too short. If too short,
		 *    fall back to async mode. If it's ok, then investigate
		 *    the received sync period. If sync period is acceptable
		 *    between sync table start_period and end_period, then
		 *    set this I_T nexus as sent offset and period.
		 *    If it's not acceptable, send back reject and fall back
		 *    to async mode.
		 */
		if (get_period < data->synct[0].period_num) {
			/*
			 * Negotiation is failed, the target send back
			 * unexpected period value.
			 */
			goto reject;
		}

		entry = nsp32_search_period_entry(data, target, get_period);

		if (entry < 0) {
			/*
			 * Target want to use long period which is not 
			 * acceptable NinjaSCSI-32Bi/UDE.
			 */
			goto reject;
		}

		/*
		 * Set new sync table and offset in this I_T nexus.
		 */
		nsp32_set_sync_entry(data, target, entry, get_offset);
	} else {
		/* Target send SDTR to initiator. */
		nsp32_dbg(NSP32_DEBUG_MSGINOCCUR, "target send SDTR");
	
		target->sync_flag |= SDTR_INITIATOR;

		/* offset: */
		if (get_offset > SYNC_OFFSET) {
			/* send back as SYNC_OFFSET */
			get_offset = SYNC_OFFSET;
		}

		/* period: */
		if (get_period < data->synct[0].period_num) {
			get_period = data->synct[0].period_num;
		}

		entry = nsp32_search_period_entry(data, target, get_period);

		if (get_offset == ASYNC_OFFSET || entry < 0) {
			nsp32_set_async(data, target);
			nsp32_build_sdtr(data, 0, ASYNC_OFFSET);
		} else {
			nsp32_set_sync_entry(data, target, entry, get_offset);
			nsp32_build_sdtr(data, get_period, get_offset);
		}
	}
	
	nsp32_dbg(NSP32_DEBUG_MSGINOCCUR, "exit");
	return;

 reject:
	/*
	 * If the current message is unacceptable, send back to the target
	 * with reject message.
	 */
	nsp32_build_reject(data);

 async:
	nsp32_set_async(data, target);	/* set as ASYNC transfer mode */

	nsp32_dbg(NSP32_DEBUG_MSGINOCCUR, "exit: set async");
	return;
}


/*
 * Search config entry number matched in sync_table from given
 * target and speed period value. If failed to search, return negative value.
 */
static int nsp32_search_period_entry(nsp32_hw_data *data,
			      struct nsp32_target *target,
			      unsigned char period)
{
	int i;

	if (target->limit_entry >= data->syncnum) {
		nsp32_msg(KERN_ERR, "limit_entry exceeds syncnum!");
		target->limit_entry = 0;
	}

	for (i=target->limit_entry; i<data->syncnum; i++) {
		if (period >= data->synct[i].start_period &&
		    period <= data->synct[i].end_period) {
				break;
		}
	}

	/*
	 * Check given period value is over the sync_table value.
	 * If so, return max value.
	 */
	if (i == data->syncnum) {
		i = -1;
	}

	return i;
}


/*
 * target <-> initiator use ASYNC transfer
 */
static void nsp32_set_async(nsp32_hw_data *data, struct nsp32_target *target)
{
	unsigned char period = data->synct[target->limit_entry].period_num;

	target->offset   = ASYNC_OFFSET;
	target->syncreg  = TO_SYNCREG(period, ASYNC_OFFSET);
	target->ackwidth = 0;

	nsp32_dbg(NSP32_DEBUG_SYNC, "set async");
}


/*
 * target <-> initiator use maximum SYNC transfer
 */
static void nsp32_set_max_sync(nsp32_hw_data *data,
			       struct nsp32_target *target,
			       unsigned char *period, unsigned char *offset)
{
	unsigned char period_num, ackwidth;

	period_num = data->synct[target->limit_entry].period_num;
	*period    = data->synct[target->limit_entry].start_period;
	ackwidth   = data->synct[target->limit_entry].ackwidth;
	*offset    = SYNC_OFFSET;

	target->syncreg  = TO_SYNCREG(period_num, *offset);
	target->ackwidth = ackwidth;
	target->offset   = *offset;
}


/*
 * target <-> initiator use entry number speed
 */
static void nsp32_set_sync_entry(nsp32_hw_data *data,
				 struct nsp32_target *target,
				 int entry, unsigned char offset)
{
	unsigned char period, ackwidth;

	period   = data->synct[entry].period_num;
	ackwidth = data->synct[entry].ackwidth;
	offset  = offset;

	target->syncreg  = TO_SYNCREG(period, offset);
	target->ackwidth = ackwidth;
	target->offset   = offset;

	nsp32_dbg(NSP32_DEBUG_SYNC, "set sync");
}


/*
 * It waits until SCSI REQ becomes assertion or negation state.
 *
 * Note: If nsp32_msgin_occur is called, we asserts SCSI ACK. Then
 *     connected target responds SCSI REQ negation.  We have to wait
 *     SCSI REQ becomes negation in order to negate SCSI ACK signal for
 *     REQ-ACK handshake.
 */
static void nsp32_wait_req(nsp32_hw_data *data, int state)
{
	unsigned int base = data->BaseAddress;
	int wait_time = 0;
	unsigned char bus;

	if (!((state == ASSERT) || (state == NEGATE))) {
		nsp32_msg(KERN_ERR, "unknown state designation");
	}
	state <<= 5; /* REQ is BIT(5) */

	do {
		bus = nsp32_read1(base, SCSI_BUS_MONITOR);
		if ((bus & BUSMON_REQ) == state) {
			nsp32_dbg(NSP32_DEBUG_WAIT, 
				  "wait_time: %d", wait_time);
			return;
		}
		udelay(1);
		wait_time++;
	} while (wait_time < REQSACK_TIMEOUT_TIME);

	nsp32_msg(KERN_WARNING, "wait REQ timeout, state: %d", state);
}

/*
 * It waits until SCSI SACK becomes assertion or negation state.
 */
static void nsp32_wait_sack(nsp32_hw_data *data, int state)
{
	unsigned int base = data->BaseAddress;
	int wait_time = 0;
	unsigned char bus;

	if (!((state == ASSERT) || (state == NEGATE))) {
		nsp32_msg(KERN_ERR, "unknown state designation");
	}
	state <<= 4; /* ACK is BIT(4) */

	do {
		bus = nsp32_read1(base, SCSI_BUS_MONITOR);
		if ((bus & BUSMON_ACK) == state) {
			nsp32_dbg(NSP32_DEBUG_WAIT,
				  "wait_time: %d", wait_time);
			return;
		}
		udelay(1);
		wait_time++;
	} while (wait_time < REQSACK_TIMEOUT_TIME);

	nsp32_msg(KERN_WARNING, "wait SACK timeout, state: %d", state);
}

/*
 * assert SCSI ACK
 *
 * Note: SCSI ACK assertion needs with ACKENB=1, AUTODIRECTION=1.
 */
static void nsp32_sack_assert(nsp32_hw_data *data)
{
	unsigned char busctrl;
	unsigned int base = data->BaseAddress;

	busctrl  = nsp32_read1(base, SCSI_BUS_CONTROL);
	busctrl	|= (BUSCTL_ACK | AUTODIRECTION | ACKENB);
	nsp32_write1(base, SCSI_BUS_CONTROL,busctrl);
}

/*
 * negate SCSI ACK
 */
static void nsp32_sack_negate(nsp32_hw_data *data)
{
	unsigned char busctrl;
	unsigned int base = data->BaseAddress;

	busctrl  = nsp32_read1(base, SCSI_BUS_CONTROL);
	busctrl	&= ~BUSCTL_ACK;
	nsp32_write1(base, SCSI_BUS_CONTROL, busctrl);
}



/*
 * getting EEPROM parameter
 */
static int nsp32_getprom_param(nsp32_hw_data *data)
{
	int vendor = data->pci_devid->vendor;
	int device = data->pci_devid->device;
	int ret, val, i;

	/*
	 * EEPROM checking.
	 */
	ret = nsp32_prom_read(data, 0x7e);
	if (ret != 0x55) {
		nsp32_msg(KERN_INFO, "No EEPROM detected: 0x%x", ret);
		return (FALSE);
	}
	ret = nsp32_prom_read(data, 0x7f);
	if (ret != 0xaa) {
		nsp32_msg(KERN_INFO, "Invalid number: 0x%x", ret);
		return (FALSE);
	}

	/*
	 * check EEPROM type
	 */
	if (vendor == PCI_VENDOR_ID_WORKBIT &&
	    device == PCI_DEVICE_ID_WORKBIT_STANDARD) {
		ret = nsp32_getprom_standard(data);
	} else if (vendor == PCI_VENDOR_ID_WORKBIT &&
		   device == PCI_DEVICE_ID_NINJASCSI_32BIB_LOGITEC) {
		ret = nsp32_getprom_new(data);
	} else if (vendor == PCI_VENDOR_ID_WORKBIT &&
		   device == PCI_DEVICE_ID_NINJASCSI_32UDE_MELCO ) {
		ret = nsp32_getprom_new(data);
	} else {
		nsp32_msg(KERN_WARNING, "Unknown EEPROM");
		ret = FALSE;
	}

	/* for debug : SPROM data full checking */
	for (i=0; i<=0x1f; i++) {
		val = nsp32_prom_read(data, i);
		nsp32_dbg(NSP32_DEBUG_EEPROM,
			  "rom address 0x%x : 0x%x", i, val);
	}

	return (ret);
}


/*
 * AT24C01A (Logitec: LHA-600S), AT24C02 (Melco Buffalo: IFC-USLP) data map:
 *
 *   ROMADDR
 *   0x00 - 0x06 :  Device Synchronous Transfer Period (SCSI ID 0 - 6) 
 *			Value 0x0: ASYNC, 0x0c: Ultra-20M, 0x19: Fast-10M
 *   0x07        :  HBA Synchronous Transfer Period
 *			Value 0: AutoSync, 1: Manual Setting
 *   0x08 - 0x0f :  Not Used? (0x0)
 *   0x10        :  Bus Termination
 * 			Value 0: Auto[ON], 1: ON, 2: OFF
 *   0x11        :  Not Used? (0)
 *   0x12        :  Bus Reset Delay Time (0x03)
 *   0x13        :  Bootable CD Support
 *			Value 0: Disable, 1: Enable
 *   0x14        :  Device Scan
 *			Bit   7  6  5  4  3  2  1  0
 *			      |  <----------------->
 * 			      |    SCSI ID: Value 0: Skip, 1: YES
 *			      |->  Value 0: ALL scan,  Value 1: Manual
 *   0x15 - 0x1b :  Not Used? (0)
 *   0x1c        :  Constant? (0x01) (clock div?)
 *   0x1d - 0x7c :  Not Used (0xff)
 *   0x7d	 :  Not Used? (0xff)
 *   0x7e        :  Constant (0x55), HBA chip revision
 *   0x7f        :  Constant (0xaa), HBA value
 */
static int nsp32_getprom_new(nsp32_hw_data *data)
{
	int ret, i;
	int auto_sync;
	struct nsp32_target *target;
	int entry;

	/*
	 * Reset time which is designated by EEPROM.
	 *
	 * TODO: Not used yet.
	 */
	data->resettime = nsp32_prom_read(data, 0x12);

	/*
	 * HBA Synchronous Transfer Period
	 *
	 * Note: auto_sync = 0: auto, 1: manual.  Ninja SCSI HBA spec says
	 *	that if auto_sync is 0 (auto), and connected SCSI devices are
	 *	same or lower than 3, then transfer speed is set as ULTRA-20M.
	 *	On the contrary if connected SCSI devices are same or higher
	 *	than 4, then transfer speed is set as FAST-10M.
	 *
	 *	I break this rule. The number of connected SCSI devices are
	 *	only ignored. If auto_sync is 0 (auto), then transfer speed is
	 *	forced as ULTRA-20M.
	 */
	ret = nsp32_prom_read(data, 0x07);
	switch (ret) {
	case 0:
		auto_sync = TRUE;
		break;
	case 1:
		auto_sync = FALSE;
		break;
	default:
		nsp32_msg(KERN_WARNING,
			  "Unsupported Auto Sync mode."
			  "Fall back to manual mode.");
		auto_sync = TRUE;
	}

	if (trans_mode == ULTRA20M_MODE) {
		auto_sync = TRUE;
	}

	/*
	 * each device Synchronous Transfer Period
	 */
	for (i=0; i<NSP32_HOST_SCSIID; i++) {
		target = &data->target[i];
		if (auto_sync == TRUE) {
			target->limit_entry = 0;   /* set as ULTRA20M */
		} else {
			ret = nsp32_prom_read(data, i);
			entry = nsp32_search_period_entry(data, target, ret);
			if (entry < 0) {
				/* search failed... set maximum speed */
				entry = 0;
			}
			target->limit_entry = entry;
		}
	}

	return (TRUE);
}


/*
 * ? (I-O Data: SC-NBD) data map:
 *
 *   ROMADDR
 *   0x00 - 0x06 :  Device Synchronous Transfer Period (SCSI ID 0 - 6) 
 *			Value 0x0: 20MB/S, 0x1: 10MB/S, 0x2: 5MB/S, 0x3: ASYNC
 *   0x07        :  0 (HBA Synchronous Transfer Period: Auto Sync)
 *   0x08 - 0x0f :  Not Used? (0x0)
 *   0x10        :  Transfer Mode
 *			Value 0: PIO, 1: Busmater
 *   0x11        :  Bus Reset Delay Time (0x00-0x20)
 *   0x12        :  Bus Termination
 * 			Value 0: Disable, 1: Enable
 *   0x13 - 0x19 :  Disconnection
 *			Value 0: Disable, 1: Enable
 *   0x1a - 0x7c :  Not Used? (0)
 *   0x7d	 :  Not Used? (0xf8)
 *   0x7e        :  Constant (0x55), HBA chip revision
 *   0x7f        :  Constant (0xaa), HBA value
 */
static int nsp32_getprom_standard(nsp32_hw_data *data)
{
	int ret, i;
	struct nsp32_target *target;
	int entry, val;

	/*
	 * Reset time which is designated by EEPROM.
	 *
	 * TODO: Not used yet.
	 */
	data->resettime = nsp32_prom_read(data, 0x11);

	/*
	 * each device Synchronous Transfer Period
	 */
	for (i=0; i<NSP32_HOST_SCSIID; i++) {
		target = &data->target[i];
		ret = nsp32_prom_read(data, i);
		switch (ret) {
		case 0:		/* 20MB/s */
			val = 0x0c;
			break;
		case 1:		/* 10MB/s */
			val = 0x19;
			break;
		case 2:		/* 5MB/s */
			val = 0x32;
			break;
		case 3:		/* ASYNC */
			val = 0x0;
			break;
		default:	/* default 20MB/s */
			val = 0x0c;
		}
		entry = nsp32_search_period_entry(data, target, val);
		if (entry < 0 || trans_mode == ULTRA20M_MODE) {
			/* search failed... set maximum speed */
			entry = 0;
		}
		target->limit_entry = entry;
	}

	return (TRUE);
}


/*
 * Atmel AT24C01A (drived in 5V) serial EEPROM routines
 */
static int nsp32_prom_read(nsp32_hw_data *data, int romaddr)
{
	int i, val;

	/* start condition */
	nsp32_prom_start(data);

	/* device address */
	nsp32_prom_write(data, 1);	/* 1 */
	nsp32_prom_write(data, 0);	/* 0 */
	nsp32_prom_write(data, 1);	/* 1 */
	nsp32_prom_write(data, 0);	/* 0 */
	nsp32_prom_write(data, 0);	/* A2: 0 (GND) */
	nsp32_prom_write(data, 0);	/* A1: 0 (GND) */
	nsp32_prom_write(data, 0);	/* A0: 0 (GND) */

	/* R/W: W for dummy write */
	nsp32_prom_write(data, 0);

	/* ack */
	nsp32_prom_write(data, 0);

	/* word address */
	for (i=7; i>=0; i--) {
		nsp32_prom_write(data, ((romaddr >> i) & 1));
	}

	/* ack */
	nsp32_prom_write(data, 0);

	/* start condition */
	nsp32_prom_start(data);

	/* device address */
	nsp32_prom_write(data, 1);	/* 1 */
	nsp32_prom_write(data, 0);	/* 0 */
	nsp32_prom_write(data, 1);	/* 1 */
	nsp32_prom_write(data, 0);	/* 0 */
	nsp32_prom_write(data, 0);	/* A2: 0 (GND) */
	nsp32_prom_write(data, 0);	/* A1: 0 (GND) */
	nsp32_prom_write(data, 0);	/* A0: 0 (GND) */

	/* R/W: R */
	nsp32_prom_write(data, 1);

	/* ack */
	nsp32_prom_write(data, 0);

	/* data... */
	val = 0;
	for (i=7; i>=0; i--) {
		val += (nsp32_prom_fetch(data) << i);
	}
	
	/* no ack */
	nsp32_prom_write(data, 1);

	/* stop condition */
	nsp32_prom_stop(data);

	return (val);
}

static void nsp32_prom_start (nsp32_hw_data *data)
{
	/* start condition */
	nsp32_prom_set(data, SCL, 1);
	nsp32_prom_set(data, SDA, 1);
	nsp32_prom_set(data, ENA, 1);	/* output mode */
	nsp32_prom_set(data, SDA, 0);	/* keeping SCL=1 and transiting
					 * SDA 1->0 is start condition */
	nsp32_prom_set(data, SCL, 0);
}

static void nsp32_prom_stop (nsp32_hw_data *data)
{
	/* stop condition */
	nsp32_prom_set(data, SCL, 1);
	nsp32_prom_set(data, SDA, 0);
	nsp32_prom_set(data, ENA, 1);	/* output mode */
	nsp32_prom_set(data, SDA, 1);
	nsp32_prom_set(data, SCL, 0);
}

static void nsp32_prom_write (nsp32_hw_data *data, int val)
{
	/* write */
	nsp32_prom_set(data, SDA, val);
	nsp32_prom_set(data, SCL, 1);
	nsp32_prom_set(data, SCL, 0);
}

static int nsp32_prom_fetch (nsp32_hw_data *data)
{
	int val;

	/* read */
	nsp32_prom_set(data, ENA, 0);	/* input mode */
	nsp32_prom_set(data, SCL, 1);
	val = nsp32_prom_get(data, SDA);
	nsp32_prom_set(data, SCL, 0);
	nsp32_prom_set(data, ENA, 1);	/* output mode */
	return (val);
}

static inline void nsp32_prom_set(nsp32_hw_data *data, int bit, int val)
{
	int cur;
	int base = data->BaseAddress;

	switch(val) {
	case 0:
		cur = nsp32_index_read1(base, SERIAL_ROM_CTL);
		nsp32_index_write1(base, SERIAL_ROM_CTL, cur & ~bit);
		break;
	case 1:
		cur = nsp32_index_read1(base, SERIAL_ROM_CTL);
		nsp32_index_write1(base, SERIAL_ROM_CTL, cur | bit);
		break;
	default:
		nsp32_msg(KERN_ERR, "val must be 0 or 1");
		return;
	}

	udelay(10);
}

static inline int nsp32_prom_get(nsp32_hw_data *data, int bit)
{
	int ret;
	int base = data->BaseAddress;

	ret = nsp32_index_read1(base, SERIAL_ROM_CTL) & bit;
	switch (ret) {
	case 0:
		ret = 0;
		break;
	case SDA:
		ret = 1;
		break;
	default:
		nsp32_msg(KERN_ERR, "return value is not appropriate");
	}

	udelay(10);

	return (ret);
}

/* end */
