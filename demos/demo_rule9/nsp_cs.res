/*======================================================================

    NinjaSCSI-3 / NinjaSCSI-32Bi PCMCIA SCSI host adapter card driver
      By: YOKOTA Hiroshi <yokota@netlab.is.tsukuba.ac.jp>

    Ver.2.8   Support 32bit MMIO mode
              Support Synchronous Data TRansfer (SDTR) mode
    Ver.2.0   Support 32bit PIO mode
    Ver.1.1.2 Fix for scatter list buffer exceeds
    Ver.1.1   Support scatter list
    Ver.0.1   Initial version

    This software may be used and distributed according to the terms of
    the GNU General Public License.

======================================================================*/

/***********************************************************************
    This driver is for these PCcards.

	I-O DATA PCSC-F	 (Workbit NinjaSCSI-3)
			"WBT", "NinjaSCSI-3", "R1.0"
	I-O DATA CBSC-II (Workbit NinjaSCSI-32Bi in 16bit mode)
			"IO DATA", "CBSC16	 ", "1"

***********************************************************************/

/*  */

#ifdef NSP_KERNEL_2_2
#include <pcmcia/config.h>
#include <pcmcia/k_compat.h>
#endif

#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/sched.h>
#include <linux/slab.h>
#include <linux/string.h>
#include <linux/timer.h>
#include <linux/ioport.h>
#include <linux/delay.h>
#include <linux/interrupt.h>
#include <linux/module.h>
#include <linux/major.h>
#include <linux/blk.h>
#include <linux/stat.h>

#include <asm/io.h>
#include <asm/irq.h>

#include "scsi.h"
#include "hosts.h"

#include <scsi/scsi.h>
#include <scsi/scsi_ioctl.h>

#include <pcmcia/version.h>
#include <pcmcia/cs_types.h>
#include <pcmcia/cs.h>
#include <pcmcia/cistpl.h>
#include <pcmcia/cisreg.h>
#include <pcmcia/ds.h>

#include "nsp_cs.h"

MODULE_AUTHOR("YOKOTA Hiroshi <yokota@netlab.is.tsukuba.ac.jp>");
MODULE_DESCRIPTION("WorkBit NinjaSCSI-3 / NinjaSCSI-32Bi(16bit) PCMCIA SCSI host adapter module $Revision: 1.1 $");
MODULE_SUPPORTED_DEVICE("sd,sr,sg,st");
#ifdef MODULE_LICENSE
MODULE_LICENSE("GPL");
#endif

#ifdef PCMCIA_DEBUG
static int pc_debug = PCMCIA_DEBUG;
MODULE_PARM(pc_debug, "i");
MODULE_PARM_DESC(pc_debug, "set debug level");
static char *version = "";
#define DEBUG(n, args...) if (pc_debug>(n)) printk(KERN_DEBUG args)
#else
#define DEBUG(n, args...) /* */
#endif

#include "nsp_io.h"

/*====================================================================*/

typedef struct scsi_info_t {
	dev_link_t             link;
	dev_node_t	       node;
	struct Scsi_Host      *host;
	int                    stop;
} scsi_info_t;


/*----------------------------------------------------------------*/

#if (KERNEL_VERSION(2,4,0) > LINUX_VERSION_CODE)
#define PROC_SCSI_NSP PROC_SCSI_IBMMCA /* bad hack... */
static struct proc_dir_entry proc_scsi_nsp = {
	PROC_SCSI_NSP, 6, "nsp_cs",
	S_IFDIR | S_IRUGO | S_IXUGO, 2
};
#endif

/*====================================================================*/
/* Parameters that can be set with 'insmod' */

static unsigned int irq_mask = 0xffff;
MODULE_PARM(irq_mask, "i");
MODULE_PARM_DESC(irq_mask, "IRQ mask bits (default: 0xffff)");

static int irq_list[4] = { -1 };
MODULE_PARM(irq_list, "1-4i");
MODULE_PARM_DESC(irq_list, "Use specified IRQ number. (default: auto select)");

static int nsp_burst_mode = 2;
MODULE_PARM(nsp_burst_mode, "i");
MODULE_PARM_DESC(nsp_burst_mode, "Burst transfer mode (0=io8, 1=io32, 2=mem32(default))");

/* Release IO ports after configuration? */
static int free_ports = 0;
MODULE_PARM(free_ports, "i");
MODULE_PARM_DESC(free_ports, "Release IO ports after configuration? (default: 0 (=no))");

/* /usr/src/linux/drivers/scsi/hosts.h */
static Scsi_Host_Template nsp_driver_template = {
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(2,4,0))
	.proc_name	         = "nsp_cs",      /* kernel 2.4 */
#else
	.proc_dir	         = &proc_scsi_nsp, /* kernel 2.2 */
#endif
	.proc_info		 = nsp_proc_info,
	.name			 = "WorkBit NinjaSCSI-3/32Bi(16bit)",
#if (LINUX_VERSION_CODE < KERNEL_VERSION(2,5,0))
	.detect			 = nsp_detect,
	.release		 = nsp_release,
#endif
	.info			 = nsp_info,
	.queuecommand		 = nsp_queuecommand,
/*	.eh_strategy_handler	 = nsp_eh_strategy,*/
/*	.eh_abort_handler	 = nsp_eh_abort,*/
/*	.eh_device_reset_handler = nsp_eh_device_reset,*/
	.eh_bus_reset_handler	 = nsp_eh_bus_reset,
	.eh_host_reset_handler	 = nsp_eh_host_reset,
	.can_queue		 = 1,
	.this_id		 = NSP_INITIATOR_ID,
	.sg_tablesize		 = SG_ALL,
	.cmd_per_lun		 = 1,
	.use_clustering		 = DISABLE_CLUSTERING,
#if (LINUX_VERSION_CODE < KERNEL_VERSION(2,5,2))
	.use_new_eh_code	 = 1,
#endif
};

static dev_link_t *dev_list = NULL;
static dev_info_t dev_info  = {"nsp_cs"};

static nsp_hw_data nsp_data;

/***********************************************************/

static int nsp_queuecommand(Scsi_Cmnd *SCpnt, void (*done)(Scsi_Cmnd *))
{
#ifdef PCMCIA_DEBUG
	/*unsigned int host_id = SCpnt->host->this_id;*/
	/*unsigned int base    = SCpnt->host->io_port;*/
	unsigned char target = SCpnt->target;
#endif
	nsp_hw_data *data = &nsp_data;

	DEBUG(0, "%s: SCpnt=0x%p target=%d lun=%d buff=0x%p bufflen=%d use_sg=%d\n",
	      __FUNCTION__, SCpnt, target, SCpnt->lun, SCpnt->request_buffer, SCpnt->request_bufflen, SCpnt->use_sg);
	//DEBUG(0, " before CurrentSC=0x%p\n", data->CurrentSC);

	if(data->CurrentSC != NULL) {
		printk(KERN_DEBUG " %s: CurrentSC!=NULL this can't be happen\n", __FUNCTION__);
		data->CurrentSC = NULL;
		SCpnt->result   = DID_BAD_TARGET << 16;
		done(SCpnt);
		return -1;
	}

	show_command(SCpnt);

	SCpnt->scsi_done	= done;
	data->CurrentSC		= SCpnt;

	SCpnt->SCp.Status	= CHECK_CONDITION;
	SCpnt->SCp.Message	= 0;
	SCpnt->SCp.have_data_in = IO_UNKNOWN;
	SCpnt->SCp.sent_command = 0;
	SCpnt->SCp.phase	= PH_UNDETERMINED;
	RESID		        = SCpnt->request_bufflen;

	/* setup scratch area
	   SCp.ptr		: buffer pointer
	   SCp.this_residual	: buffer length
	   SCp.buffer		: next buffer
	   SCp.buffers_residual : left buffers in list
	   SCp.phase		: current state of the command */
	if (SCpnt->use_sg) {
		SCpnt->SCp.buffer	    = (struct scatterlist *) SCpnt->request_buffer;
		SCpnt->SCp.ptr		    = BUFFER_ADDR;
		SCpnt->SCp.this_residual    = SCpnt->SCp.buffer->length;
		SCpnt->SCp.buffers_residual = SCpnt->use_sg - 1;
	} else {
		SCpnt->SCp.ptr		    = (char *) SCpnt->request_buffer;
		SCpnt->SCp.this_residual    = SCpnt->request_bufflen;
		SCpnt->SCp.buffer	    = NULL;
		SCpnt->SCp.buffers_residual = 0;
	}

	if(nsphw_start_selection(SCpnt, data) == FALSE) {
		DEBUG(0, " selection fail\n");
		data->CurrentSC = NULL;
		SCpnt->result   = DID_NO_CONNECT << 16;
		done(SCpnt);
		return -1;
	}


	//DEBUG(0, "%s: out\n", __FUNCTION__);
	return 0;
}

/*
 * setup PIO FIFO transfer mode and enable/disable to data out
 */
static void nsp_setup_fifo(nsp_hw_data *data, int enabled)
{
	unsigned int  base = data->BaseAddress;
	unsigned char transfer_mode_reg;

	//DEBUG(0, "%s: enabled=%d\n", __FUNCTION__, enabled);

	if (enabled != FALSE) {
		transfer_mode_reg = TRANSFER_GO | BRAIND;
	} else {
		transfer_mode_reg = 0;
	}

	transfer_mode_reg |= data->TransferMode;

	nsp_index_write(base, TRANSFERMODE, transfer_mode_reg);
}

static void nsphw_init_sync(nsp_hw_data *data)
{
	sync_data tmp_sync = { .SyncNegotiation = SYNC_NOT_YET,
			       .SyncPeriod      = 0,
			       .SyncOffset      = 0
	};
	int i;

	/* setup sync data */
	for ( i = 0; i < NUMBER(data->Sync); i++ ) {
		data->Sync[i] = tmp_sync;
	}
}

/*
 * Initialize Ninja hardware
 */
static int nsphw_init(nsp_hw_data *data)
{
	unsigned int base     = data->BaseAddress;

	DEBUG(0, "%s: in base=0x%x\n", __FUNCTION__, base);

	data->ScsiClockDiv = CLOCK_40M | FAST_20;
	data->CurrentSC    = NULL;
	data->FifoCount    = 0;
	data->TransferMode = MODE_IO8;

	nsphw_init_sync(data);

	/* block all interrupts */
	nsp_write(base,	      IRQCONTROL,   IRQCONTROL_ALLMASK);

	/* setup SCSI interface */
	nsp_write(base,	      IFSELECT,	    IF_IFSEL);

	nsp_index_write(base, SCSIIRQMODE,  0);

	nsp_index_write(base, TRANSFERMODE, MODE_IO8);
	nsp_index_write(base, CLOCKDIV,	    data->ScsiClockDiv);

	nsp_index_write(base, PARITYCTRL,   0);
	nsp_index_write(base, POINTERCLR,   POINTER_CLEAR     |
					    ACK_COUNTER_CLEAR |
					    REQ_COUNTER_CLEAR |
					    HOST_COUNTER_CLEAR);

	/* setup fifo asic */
	nsp_write(base,	      IFSELECT,	    IF_REGSEL);
	nsp_index_write(base, TERMPWRCTRL,  0);
	if ((nsp_index_read(base, OTHERCONTROL) & TPWR_SENSE) == 0) {
		printk(KERN_INFO "nsp_cs: terminator power on\n");
		nsp_index_write(base, TERMPWRCTRL, POWER_ON);
	}

	nsp_index_write(base, TIMERCOUNT,   0);
	nsp_index_write(base, TIMERCOUNT,   0); /* requires 2 times!! */

	nsp_index_write(base, SYNCREG,	    0);
	nsp_index_write(base, ACKWIDTH,	    0);

	/* enable interrupts and ack them */
	nsp_index_write(base, SCSIIRQMODE,  SCSI_PHASE_CHANGE_EI |
					    RESELECT_EI		 |
					    SCSI_RESET_IRQ_EI	 );
	nsp_write(base,	      IRQCONTROL,   IRQCONTROL_ALLCLEAR);

	nsp_setup_fifo(data, FALSE);

	return TRUE;
}

/*
 * Start selection phase
 */
static unsigned int nsphw_start_selection(Scsi_Cmnd   *SCpnt,
					  nsp_hw_data *data)
{
	unsigned int  host_id	 = SCpnt->device->host->this_id;
	unsigned int  base	 = SCpnt->device->host->io_port;
	unsigned char target	 = SCpnt->device->id;
	int	      time_out;
	unsigned char phase, arbit;

	//DEBUG(0, "%s:in\n", __FUNCTION__);

	phase = nsp_index_read(base, SCSIBUSMON);
	if(phase != BUSMON_BUS_FREE) {
		//DEBUG(0, " bus busy\n");
		return FALSE;
	}

	/* start arbitration */
	//DEBUG(0, " start arbit\n");
	SCpnt->SCp.phase = PH_ARBSTART;
	nsp_index_write(base, SETARBIT, ARBIT_GO);

	time_out = 1000;
	do {
		/* XXX: what a stupid chip! */
		arbit = nsp_index_read(base, ARBITSTATUS);
		//DEBUG(0, " arbit=%d, wait_count=%d\n", arbit, wait_count);
		udelay(1); /* hold 1.2us */
	} while((arbit & (ARBIT_WIN | ARBIT_FAIL)) == 0 &&
		(time_out-- != 0));

	if((arbit & ARBIT_WIN) == 0) {
		//DEBUG(0, " arbit fail\n");
		nsp_index_write(base, SETARBIT, ARBIT_FLAG_CLEAR);
		return FALSE;
	}

	/* assert select line */
	//DEBUG(0, " assert SEL line\n");
	SCpnt->SCp.phase = PH_SELSTART;
	udelay(3);
	nsp_index_write(base, SCSIDATALATCH, BIT(host_id) | BIT(target));
	nsp_index_write(base, SCSIBUSCTRL,   SCSI_SEL | SCSI_BSY                    | SCSI_ATN);
	udelay(3);
	nsp_index_write(base, SCSIBUSCTRL,   SCSI_SEL | SCSI_BSY | SCSI_DATAOUT_ENB | SCSI_ATN);
	nsp_index_write(base, SETARBIT,	     ARBIT_FLAG_CLEAR);
	udelay(3);
	nsp_index_write(base, SCSIBUSCTRL,   SCSI_SEL            | SCSI_DATAOUT_ENB | SCSI_ATN);

	/* check selection timeout */
	nsp_start_timer(SCpnt, data, 1000/51);
	data->SelectionTimeOut = 1;

	return TRUE;
}

struct nsp_sync_table {
	unsigned int min_period;
	unsigned int max_period;
	unsigned int chip_period;
	unsigned int ack_width;
};

static struct nsp_sync_table nsp_sync_table_40M[] = {
	{0x0c,0x0c,0x1,0},	/* 20MB	 50ns*/
	{0x19,0x19,0x3,1},	/* 10MB	 100ns*/ 
	{0x1a,0x25,0x5,2},	/* 7.5MB 150ns*/ 
	{0x26,0x32,0x7,3},	/* 5MB	 200ns*/
	{0x0, 0,   0,  0}
};

static struct nsp_sync_table nsp_sync_table_20M[] = {
	{0x19,0x19,0x1,0},	/* 10MB	 100ns*/ 
	{0x1a,0x25,0x2,0},	/* 7.5MB 150ns*/ 
	{0x26,0x32,0x3,1},	/* 5MB	 200ns*/
	{0x0, 0,   0,  0}
};

/*
 * setup synchronous data transfer mode
 */
static int nsp_msg(Scsi_Cmnd *SCpnt, nsp_hw_data *data)
{
	unsigned char	       target = SCpnt->device->id;
//	unsigned char	       lun    = SCpnt->lun;
	sync_data	      *sync   = &(data->Sync[target]);
	struct nsp_sync_table *sync_table;
	unsigned int	       period, offset;
	int		       i;


	DEBUG(0, "%s:\n", __FUNCTION__);

	period = sync->SyncPeriod;
	offset = sync->SyncOffset;

	DEBUG(0, " period=0x%x, offset=0x%x\n", period, offset);

	if ((data->ScsiClockDiv & (BIT(0)|BIT(1))) == CLOCK_20M) {
		sync_table = &nsp_sync_table_20M[0];
	} else {
		sync_table = &nsp_sync_table_40M[0];
	}

	for ( i = 0; sync_table->max_period != 0; i++, sync_table++) {
		if ( period >= sync_table->min_period &&
		     period <= sync_table->max_period	 ) {
			break;
		}
	}

	if (period != 0 && sync_table->max_period == 0) {
		/*
		 * No proper period/offset found
		 */
		DEBUG(0, " no proper period/offset\n");

		sync->SyncPeriod      = 0;
		sync->SyncOffset      = 0;
		sync->SyncRegister    = 0;
		sync->AckWidth	      = 0;

		return FALSE;
	}

	sync->SyncRegister    = (sync_table->chip_period << SYNCREG_PERIOD_SHIFT) |
		                (offset & SYNCREG_OFFSET_MASK);
	sync->AckWidth	      = sync_table->ack_width;

	DEBUG(0, " sync_reg=0x%x, ack_width=0x%x\n", sync->SyncRegister, sync->AckWidth);

	return TRUE;
}


/*
 * start ninja hardware timer
 */
static void nsp_start_timer(Scsi_Cmnd *SCpnt, nsp_hw_data *data, int time)
{
	unsigned int base = SCpnt->device->host->io_port;

	//DEBUG(0, "%s: in SCpnt=0x%p, time=%d\n", __FUNCTION__, SCpnt, time);
	data->TimerCount = time;
	nsp_index_write(base, TIMERCOUNT, time);
}

/*
 * wait for bus phase change
 */
static int nsp_negate_signal(Scsi_Cmnd *SCpnt, unsigned char mask, char *str)
{
	unsigned int  base = SCpnt->device->host->io_port;
	unsigned char reg;
	int	      time_out;

	//DEBUG(0, "%s:\n", __FUNCTION__);

	time_out = 100;

	do {
		reg = nsp_index_read(base, SCSIBUSMON);
		if (reg == 0xff) {
			break;
		}
	} while ((time_out-- != 0) && (reg & mask) != 0);

	if (time_out == 0) {
		printk(KERN_DEBUG "%s:: %s signal off timeut\n", __FUNCTION__, str);
	}

	return 0;
}

/*
 * expect Ninja Irq
 */
static int nsp_expect_signal(Scsi_Cmnd	   *SCpnt,
			     unsigned char  current_phase,
			     unsigned char  mask)
{
	unsigned int  base	 = SCpnt->device->host->io_port;
	int	      time_out;
	unsigned char phase, i_src;

	//DEBUG(0, "%s: current_phase=0x%x, mask=0x%x\n", __FUNCTION__, current_phase, mask);

	time_out = 100;
	do {
		phase = nsp_index_read(base, SCSIBUSMON);
		if (phase == 0xff) {
			//DEBUG(0, " ret -1\n");
			return -1;
		}
		i_src = nsp_read(base, IRQSTATUS);
		if (i_src & IRQSTATUS_SCSI) {
			//DEBUG(0, " ret 0 found scsi signal\n");
			return 0;
		}
		if ((phase & mask) != 0 && (phase & BUSMON_PHASE_MASK) == current_phase) {
			//DEBUG(0, " ret 1 phase=0x%x\n", phase);
			return 1;
		}
	} while(time_out-- != 0);

	//DEBUG(0, "%s: timeout\n", __FUNCTION__);
	return -1;
}

/*
 * transfer SCSI message
 */
static int nsp_xfer(Scsi_Cmnd *SCpnt, nsp_hw_data *data, int phase)
{
	unsigned int  base = SCpnt->device->host->io_port;
	char	     *buf  = data->MsgBuffer;
	int	      len  = MIN(MSGBUF_SIZE, data->MsgLen);
	int	      ptr;
	int	      ret;

	//DEBUG(0, "%s:\n", __FUNCTION__);
	for (ptr = 0; len > 0; len --, ptr ++) {

		ret = nsp_expect_signal(SCpnt, phase, BUSMON_REQ);
		if (ret <= 0) {
			DEBUG(0, " xfer quit\n");
			return 0;
		}

		/* if last byte, negate ATN */
		if (len == 1 && SCpnt->SCp.phase == PH_MSG_OUT) {
			nsp_index_write(base, SCSIBUSCTRL, AUTODIRECTION | ACKENB);
		}

		/* read & write message */
		if (phase & BUSMON_IO) {
			DEBUG(0, " read msg\n");
			buf[ptr] = nsp_index_read(base, SCSIDATAWITHACK);
		} else {
			DEBUG(0, " write msg\n");
			nsp_index_write(base, SCSIDATAWITHACK, buf[ptr]);
		}
		nsp_negate_signal(SCpnt, BUSMON_ACK, "xfer<ack>");

	}
	return len;
}

/*
 * get extra SCSI data from fifo
 */
static int nsp_dataphase_bypass(Scsi_Cmnd *SCpnt, nsp_hw_data *data)
{
	unsigned int count;

	//DEBUG(0, "%s:\n", __FUNCTION__);

	if (SCpnt->SCp.have_data_in != IO_IN) {
		return 0;
	}

	count = nsp_fifo_count(SCpnt);
	if (data->FifoCount == count) {
		//DEBUG(0, " not use bypass quirk\n");
		return 0;
	}

	/*
	 * XXX: NSP_QUIRK
	 * data phase skip only occurs in case of SCSI_LOW_READ
	 */
	DEBUG(0, " use bypass quirk\n");
	SCpnt->SCp.phase = PH_DATA;
	nsp_pio_read(SCpnt, data);
	nsp_setup_fifo(data, FALSE);

	return 0;
}

/*
 * accept reselection
 */
static int nsp_reselected(Scsi_Cmnd *SCpnt, nsp_hw_data *data)
{
	unsigned int  base = SCpnt->device->host->io_port;
	unsigned char reg;

	//DEBUG(0, "%s:\n", __FUNCTION__);

	nsp_negate_signal(SCpnt, BUSMON_SEL, "reselect<SEL>");

	nsp_nexus(SCpnt, data);
	reg = nsp_index_read(base, SCSIBUSCTRL) & ~(SCSI_BSY | SCSI_ATN);
	nsp_index_write(base, SCSIBUSCTRL, reg);
	nsp_index_write(base, SCSIBUSCTRL, reg | AUTODIRECTION | ACKENB);

	return TRUE;
}

/*
 * count how many data transferred
 */
static int nsp_fifo_count(Scsi_Cmnd *SCpnt)
{
	unsigned int base = SCpnt->device->host->io_port;
	unsigned int count;
	unsigned int l, m, h, dummy;

	nsp_index_write(base, POINTERCLR, POINTER_CLEAR | ACK_COUNTER);

	l     = nsp_index_read(base, TRANSFERCOUNT);
	m     = nsp_index_read(base, TRANSFERCOUNT);
	h     = nsp_index_read(base, TRANSFERCOUNT);
	dummy = nsp_index_read(base, TRANSFERCOUNT);

	count = (h << 16) | (m << 8) | (l << 0);

	//DEBUG(0, "%s: =0x%x\n", __FUNCTION__, count);

	return count;
}

/* fifo size */
#define RFIFO_CRIT 64
#define WFIFO_CRIT 64

/*
 * read data in DATA IN phase
 */
static void nsp_pio_read(Scsi_Cmnd *SCpnt, nsp_hw_data *data)
{
	unsigned int  base      = SCpnt->device->host->io_port;
	unsigned long mmio_base = SCpnt->device->host->base;
	long	      time_out;
	int	      ocount, res;
	unsigned char stat, fifo_stat;

	ocount = data->FifoCount;

	DEBUG(0, "%s: in SCpnt=0x%p resid=%d ocount=%d ptr=0x%p this_residual=%d buffers=0x%p nbuf=%d\n",
	      __FUNCTION__, SCpnt, RESID, ocount, SCpnt->SCp.ptr, SCpnt->SCp.this_residual, SCpnt->SCp.buffer, SCpnt->SCp.buffers_residual);

	time_out = 1000;

	while ((time_out-- != 0) &&
	       (SCpnt->SCp.this_residual > 0 || SCpnt->SCp.buffers_residual > 0 ) ) {

		stat = nsp_index_read(base, SCSIBUSMON);
		stat &= BUSMON_PHASE_MASK;


		res = nsp_fifo_count(SCpnt) - ocount;
		//DEBUG(0, " ptr=0x%p this=0x%x ocount=0x%x res=0x%x\n", SCpnt->SCp.ptr, SCpnt->SCp.this_residual, ocount, res);
		if (res == 0) { /* if some data available ? */
			if (stat == BUSPHASE_DATA_IN) { /* phase changed? */
				//DEBUG(0, " wait for data this=%d\n", SCpnt->SCp.this_residual);
				continue;
			} else {
				DEBUG(0, " phase changed stat=0x%x\n", stat);
				break;
			}
		}

		fifo_stat = nsp_read(base, FIFOSTATUS);
		if ((fifo_stat & FIFOSTATUS_FULL_EMPTY) == 0 &&
		    stat                                == BUSPHASE_DATA_IN) {
			continue;
		}

		res = MIN(res, SCpnt->SCp.this_residual);

		switch (data->TransferMode) {
		case MODE_IO32:
			res &= ~(BIT(1)|BIT(0)); /* align 4 */
			nsp_fifo32_read(base, SCpnt->SCp.ptr, res >> 2);
			break;
		case MODE_IO8:
			nsp_fifo8_read (base, SCpnt->SCp.ptr, res     );
			break;

		case MODE_MEM32:
			res &= ~(BIT(1)|BIT(0)); /* align 4 */
			nsp_mmio_fifo32_read(mmio_base, SCpnt->SCp.ptr, res >> 2);
			break;

		default:
			DEBUG(0, "unknown read mode\n");
			return;
		}

		RESID			 -= res;
		SCpnt->SCp.ptr		 += res;
		SCpnt->SCp.this_residual -= res;
		ocount			 += res;
		//DEBUG(0, " ptr=0x%p this_residual=0x%x ocount=0x%x\n", SCpnt->SCp.ptr, SCpnt->SCp.this_residual, ocount);

		/* go to next scatter list if available */
		if (SCpnt->SCp.this_residual	== 0 &&
		    SCpnt->SCp.buffers_residual != 0 ) {
			//DEBUG(0, " scatterlist next timeout=%d\n", time_out);
			SCpnt->SCp.buffers_residual--;
			SCpnt->SCp.buffer++;
			SCpnt->SCp.ptr		 = BUFFER_ADDR;
			SCpnt->SCp.this_residual = SCpnt->SCp.buffer->length;
			time_out = 1000;

			//DEBUG(0, "page: 0x%p, off: 0x%x\n", SCpnt->SCp.buffer->page, SCpnt->SCp.buffer->offset);
		}
	}

	data->FifoCount = ocount;

	if (time_out == 0) {
		printk(KERN_DEBUG "%s: pio read timeout resid=%d this_residual=%d buffers_residual=%d\n",
		       __FUNCTION__, RESID, SCpnt->SCp.this_residual, SCpnt->SCp.buffers_residual);
	}
	DEBUG(0, " read ocount=0x%x\n", ocount);
}

/*
 * write data in DATA OUT phase
 */
static void nsp_pio_write(Scsi_Cmnd *SCpnt, nsp_hw_data *data)
{
	unsigned int  base     = SCpnt->device->host->io_port;
	unsigned long mmio_base = SCpnt->device->host->base;
	int	      time_out;
	int           ocount, res;
	unsigned char stat;

	ocount	 = data->FifoCount;

	DEBUG(0, "%s: in fifocount=%d ptr=0x%p this_residual=%d buffers=0x%p nbuf=%d resid=0x%x\n",
	      __FUNCTION__, data->FifoCount, SCpnt->SCp.ptr, SCpnt->SCp.this_residual, SCpnt->SCp.buffer, SCpnt->SCp.buffers_residual, RESID);

	time_out = 1000;

	while ((time_out-- != 0) &&
	       (SCpnt->SCp.this_residual > 0 || SCpnt->SCp.buffers_residual > 0)) {
		stat = nsp_index_read(base, SCSIBUSMON);
		stat &= BUSMON_PHASE_MASK;

		if (stat != BUSPHASE_DATA_OUT) {
			res = ocount - nsp_fifo_count(SCpnt);

			DEBUG(0, " phase changed stat=0x%x, res=%d\n", stat, res);
			/* Put back pointer */
			RESID			 += res;
			SCpnt->SCp.ptr		 -= res;
			SCpnt->SCp.this_residual += res;
			ocount			 -= res;

			break;
		}

		res = ocount - nsp_fifo_count(SCpnt);
		if (res > 0) { /* write all data? */
			DEBUG(0, " wait for all data out. ocount=0x%x res=%d\n", ocount, res);
			continue;
		}

		res = MIN(SCpnt->SCp.this_residual, WFIFO_CRIT);

		//DEBUG(0, " ptr=0x%p this=0x%x res=0x%x\n", SCpnt->SCp.ptr, SCpnt->SCp.this_residual, res);
		switch (data->TransferMode) {
		case MODE_IO32:
			res &= ~(BIT(1)|BIT(0)); /* align 4 */
			nsp_fifo32_write(base, SCpnt->SCp.ptr, res >> 2);
			break;
		case MODE_IO8:
			nsp_fifo8_write (base, SCpnt->SCp.ptr, res     );
			break;

		case MODE_MEM32:
			res &= ~(BIT(1)|BIT(0)); /* align 4 */
			nsp_mmio_fifo32_write(mmio_base, SCpnt->SCp.ptr, res >> 2);
			break;

		default:
			DEBUG(0, "unknown write mode\n");
			break;
		}

		RESID			 -= res;
		SCpnt->SCp.ptr		 += res;
		SCpnt->SCp.this_residual -= res;
		ocount			 += res;

		/* go to next scatter list if available */
		if (SCpnt->SCp.this_residual	== 0 &&
		    SCpnt->SCp.buffers_residual != 0 ) {
			//DEBUG(0, " scatterlist next\n");
			SCpnt->SCp.buffers_residual--;
			SCpnt->SCp.buffer++;
			SCpnt->SCp.ptr		 = BUFFER_ADDR;
			SCpnt->SCp.this_residual = SCpnt->SCp.buffer->length;
			time_out = 1000;
		}
	}

	data->FifoCount = ocount;

	if (time_out == 0) {
		printk(KERN_DEBUG "%s: pio write timeout resid=0x%x\n", __FUNCTION__, RESID);
	}
	DEBUG(0, " write ocount=0x%x\n", ocount);
}

#undef RFIFO_CRIT
#undef WFIFO_CRIT

/*
 * setup synchronous/asynchronous data transfer mode
 */
static int nsp_nexus(Scsi_Cmnd *SCpnt, nsp_hw_data *data)
{
	unsigned int   base   = SCpnt->device->host->io_port;
	unsigned char  target = SCpnt->device->id;
//	unsigned char  lun    = SCpnt->lun;
	sync_data     *sync   = &(data->Sync[target]);

	//DEBUG(0, "%s: in SCpnt=0x%p\n", __FUNCTION__, SCpnt);

	/* setup synch transfer registers */
	nsp_index_write(base, SYNCREG,	sync->SyncRegister);
	nsp_index_write(base, ACKWIDTH, sync->AckWidth);

	if (SCpnt->use_sg == 0     ||
	    RESID % 4 != 0         ||
	    RESID     <= PAGE_SIZE ) {
		data->TransferMode = MODE_IO8;
	} else if (nsp_burst_mode == BURST_MEM32) {
		data->TransferMode = MODE_MEM32;
	} else if (nsp_burst_mode == BURST_IO32) {
		data->TransferMode = MODE_IO32;
	} else {
		data->TransferMode = MODE_IO8;
	}

	/* setup pdma fifo */
	nsp_setup_fifo(data, TRUE);

	/* clear ack counter */
	data->FifoCount = 0;
	nsp_index_write(base, POINTERCLR, POINTER_CLEAR	    |
					  ACK_COUNTER_CLEAR |
					  REQ_COUNTER_CLEAR |
					  HOST_COUNTER_CLEAR);

	return 0;
}

#include "nsp_message.c"
/*
 * interrupt handler
 */
static void nspintr(int irq, void *dev_id, struct pt_regs *regs)
{
	unsigned int   base;
	unsigned char  i_src, irq_phase, phase;
	Scsi_Cmnd     *tmpSC;
	unsigned char  target, lun;
	unsigned int  *sync_neg;
	int            i, tmp;
	nsp_hw_data   *data = dev_id;

	//printk("&nsp_data=0x%p, dev_id=0x%p\n", &nsp_data, dev_id);

	base = data->BaseAddress;
	//DEBUG(0, " base=0x%x\n", base);

	/*
	 * interrupt check
	 */
	nsp_write(base, IRQCONTROL, IRQCONTROL_IRQDISABLE);
	i_src = nsp_read(base, IRQSTATUS);
	//DEBUG(0, " i_src=0x%x\n", i_src);
	if ((i_src == 0xff) || ((i_src & IRQSTATUS_MASK) == 0)) {
		nsp_write(base, IRQCONTROL, 0);
		//DEBUG(0, " no irq/shared irq\n");
		return;
	}


	/* XXX: IMPORTANT
	 * Do not read an irq_phase register if no scsi phase interrupt.
	 * Unless, you should lose a scsi phase interrupt.
	 */
	phase = nsp_index_read(base, SCSIBUSMON);
	if((i_src & IRQSTATUS_SCSI) != 0) {
		irq_phase = nsp_index_read(base, IRQPHASESENCE);
	} else {
		irq_phase = 0;
	}

	//DEBUG(0, " irq_phase=0x%x\n", irq_phase);

	/*
	 * timer interrupt handler (scsi vs timer interrupts)
	 */
	//DEBUG(0, " timercount=%d\n", data->TimerCount);
	if (data->TimerCount != 0) {
		//DEBUG(0, " stop timer\n");
		nsp_index_write(base, TIMERCOUNT, 0);
		nsp_index_write(base, TIMERCOUNT, 0);
		data->TimerCount = 0;
	}

	if ((i_src & IRQSTATUS_MASK) == IRQSTATUS_TIMER &&
	    data->SelectionTimeOut == 0) {
		//DEBUG(0, " timer start\n");
		nsp_write(base, IRQCONTROL, IRQCONTROL_TIMER_CLEAR);
		return;
	}

	nsp_write(base, IRQCONTROL, IRQCONTROL_TIMER_CLEAR | IRQCONTROL_FIFO_CLEAR);

	if (data->CurrentSC == NULL) {
		printk(KERN_DEBUG "%s: CurrentSC==NULL irq_status=0x%x phase=0x%x irq_phase=0x%x this can't be happen\n", __FUNCTION__, i_src, phase, irq_phase);
		return;
	} else {
		tmpSC    = data->CurrentSC;
		target   = tmpSC->device->id;
		lun      = tmpSC->device->lun;
		sync_neg = &(data->Sync[target].SyncNegotiation);
	}

	/*
	 * parse hardware SCSI irq reasons register
	 */
	if ((i_src & IRQSTATUS_SCSI) != 0) {
		if ((irq_phase & SCSI_RESET_IRQ) != 0) {
			printk(KERN_DEBUG " %s: bus reset (power off?)\n", __FUNCTION__);
			*sync_neg          = SYNC_NOT_YET;
			data->CurrentSC    = NULL;
			tmpSC->result	   = (DID_RESET                   << 16) |
				             ((tmpSC->SCp.Message & 0xff) <<  8) |
				             ((tmpSC->SCp.Status  & 0xff) <<  0);
			tmpSC->scsi_done(tmpSC);
			return;
		}

		if ((irq_phase & RESELECT_IRQ) != 0) {
			DEBUG(0, " reselect\n");
			nsp_write(base, IRQCONTROL, IRQCONTROL_RESELECT_CLEAR);
			if (nsp_reselected(tmpSC, data) != FALSE) {
				return;
			}
		}

		if ((irq_phase & (PHASE_CHANGE_IRQ | LATCHED_BUS_FREE)) == 0) {
			return; 
		}
	}

	//show_phase(tmpSC);

	switch(tmpSC->SCp.phase) {
	case PH_SELSTART:
		//*sync_neg = SYNC_NOT_YET;
		if ((phase & BUSMON_BSY) == 0) {
			//DEBUG(0, " selection count=%d\n", data->SelectionTimeOut);
			if (data->SelectionTimeOut >= NSP_SELTIMEOUT) {
				DEBUG(0, " selection time out\n");
				data->SelectionTimeOut = 0;
				nsp_index_write(base, SCSIBUSCTRL, 0);

				data->CurrentSC = NULL;
				tmpSC->result   = DID_NO_CONNECT << 16;
				tmpSC->scsi_done(tmpSC);

				return;
			}
			data->SelectionTimeOut += 1;
			nsp_start_timer(tmpSC, data, 1000/51);
			return;
		}

		/* attention assert */
		//DEBUG(0, " attention assert\n");
		data->SelectionTimeOut = 0;
		tmpSC->SCp.phase       = PH_SELECTED;
		nsp_index_write(base, SCSIBUSCTRL, SCSI_ATN);
		udelay(1);
		nsp_index_write(base, SCSIBUSCTRL, SCSI_ATN | AUTODIRECTION | ACKENB);
		return;

		break;

	case PH_RESELECT:
		//DEBUG(0, " phase reselect\n");
		//*sync_neg = SYNC_NOT_YET;
		if ((phase & BUSMON_PHASE_MASK) != BUSPHASE_MESSAGE_IN) {

			data->CurrentSC = NULL;
			tmpSC->result	= DID_ABORT << 16;
			tmpSC->scsi_done(tmpSC);
			return;
		}
		/* fall thru */
	default:
		if ((i_src & (IRQSTATUS_SCSI | IRQSTATUS_FIFO)) == 0) {
			return;
		}
		break;
	}

	/*
	 * SCSI sequencer
	 */
	//DEBUG(0, " start scsi seq\n");

	/* normal disconnect */
	if (((tmpSC->SCp.phase == PH_MSG_IN) || (tmpSC->SCp.phase == PH_MSG_OUT)) &&
	    (irq_phase & LATCHED_BUS_FREE) != 0 ) {
		DEBUG(0, " normal disconnect i_src=0x%x, phase=0x%x, irq_phase=0x%x\n", i_src, phase, irq_phase);
		if ((tmpSC->SCp.Message == MSG_COMMAND_COMPLETE)) {     /* all command complete and return status */
			//*sync_neg       = SYNC_NOT_YET;
			data->CurrentSC = NULL;
			tmpSC->result = (DID_OK		             << 16) |
					((tmpSC->SCp.Message & 0xff) <<  8) |
					((tmpSC->SCp.Status  & 0xff) <<  0);
			DEBUG(0, " command complete result=0x%x\n", tmpSC->result);
			tmpSC->scsi_done(tmpSC);

			return;
		}

		return;
	}


	/* check unexpected bus free state */
	if (phase == 0) {
		printk(KERN_DEBUG " %s: unexpected bus free. i_src=0x%x, phase=0x%x, irq_phase=0x%x\n", __FUNCTION__, i_src, phase, irq_phase);

		*sync_neg       = SYNC_NOT_YET;
		data->CurrentSC = NULL;
		tmpSC->result   = DID_ERROR << 16;
		tmpSC->scsi_done(tmpSC);
		return;
	}

	switch (phase & BUSMON_PHASE_MASK) {
	case BUSPHASE_COMMAND:
		DEBUG(0, " BUSPHASE_COMMAND\n");
		if ((phase & BUSMON_REQ) == 0) {
			DEBUG(0, " REQ == 0\n");
			return;
		}

		tmpSC->SCp.phase = PH_COMMAND;

		nsp_nexus(tmpSC, data);

		/* write scsi command */
		DEBUG(0, " cmd_len=%d\n", tmpSC->cmd_len);
		nsp_index_write(base, COMMANDCTRL, CLEAR_COMMAND_POINTER);
		for (i = 0; i < tmpSC->cmd_len; i++) {
			nsp_index_write(base, COMMANDDATA, tmpSC->cmnd[i]);
		}
		nsp_index_write(base, COMMANDCTRL, CLEAR_COMMAND_POINTER | AUTO_COMMAND_GO);
		break;

	case BUSPHASE_DATA_OUT:
		DEBUG(0, " BUSPHASE_DATA_OUT\n");

		tmpSC->SCp.phase = PH_DATA;
		tmpSC->SCp.have_data_in = IO_OUT;

		nsp_pio_write(tmpSC, data);

		break;

	case BUSPHASE_DATA_IN:
		DEBUG(0, " BUSPHASE_DATA_IN\n");

		tmpSC->SCp.phase = PH_DATA;
		tmpSC->SCp.have_data_in = IO_IN;

		nsp_pio_read(tmpSC, data);

		break;

	case BUSPHASE_STATUS:
		nsp_dataphase_bypass(tmpSC, data);
		DEBUG(0, " BUSPHASE_STATUS\n");

		tmpSC->SCp.phase = PH_STATUS;

		tmpSC->SCp.Status = nsp_index_read(base, SCSIDATAWITHACK);
		DEBUG(0, " message=0x%x status=0x%x\n", tmpSC->SCp.Message, tmpSC->SCp.Status);

		break;

	case BUSPHASE_MESSAGE_OUT:
		DEBUG(0, " BUSPHASE_MESSAGE_OUT\n");
		if ((phase & BUSMON_REQ) == 0) {
			goto timer_out;
		}

		tmpSC->SCp.phase = PH_MSG_OUT;

		data->MsgLen = i = 0;
		data->MsgBuffer[i] = IDENTIFY(TRUE, lun); i++;

		if (*sync_neg == SYNC_NOT_YET) {
			data->Sync[target].SyncPeriod = 0;
			data->Sync[target].SyncOffset = 0;

			/**/
			data->MsgBuffer[i] = MSG_EXTENDED; i++;
			data->MsgBuffer[i] = 3;            i++;
			data->MsgBuffer[i] = MSG_EXT_SDTR; i++;
			data->MsgBuffer[i] = 0x0c;         i++;
			data->MsgBuffer[i] = 15;           i++;
			/**/
		}
		data->MsgLen = i;

		nsp_msg(tmpSC, data);
		show_message(data);
		nsp_message_out(tmpSC, data);
		break;

	case BUSPHASE_MESSAGE_IN:
		nsp_dataphase_bypass(tmpSC, data);
		DEBUG(0, " BUSPHASE_MESSAGE_IN\n");
		if ((phase & BUSMON_REQ) == 0) {
			goto timer_out;
		}

		tmpSC->SCp.phase = PH_MSG_IN;
		nsp_message_in(tmpSC, data);

		/**/
		if (*sync_neg == SYNC_NOT_YET) {
			//printk("%d,%d\n",target,lun);

			if (data->MsgLen       >= 5            &&
			    data->MsgBuffer[0] == MSG_EXTENDED &&
			    data->MsgBuffer[1] == 3            &&
			    data->MsgBuffer[2] == MSG_EXT_SDTR ) {
				data->Sync[target].SyncPeriod = data->MsgBuffer[3];
				data->Sync[target].SyncOffset = data->MsgBuffer[4];
				//printk("sync ok, %d %d\n", data->MsgBuffer[3], data->MsgBuffer[4]);
				*sync_neg = SYNC_OK;
			} else {
				data->Sync[target].SyncPeriod = 0;
				data->Sync[target].SyncOffset = 0;
				*sync_neg = SYNC_NG;
			}
			nsp_msg(tmpSC, data);
		}
		/**/

		/* search last messeage byte */
		tmp = -1;
		for (i = 0; i < data->MsgLen; i++) {
			tmp = data->MsgBuffer[i];
			if (data->MsgBuffer[i] == MSG_EXTENDED) {
				i += (1 + data->MsgBuffer[i+1]);
			}
		}
		tmpSC->SCp.Message = tmp;

		DEBUG(0, " message=0x%x len=%d\n", tmpSC->SCp.Message, data->MsgLen);
		show_message(data);

		break;

	case BUSPHASE_SELECT:
	default:
		DEBUG(0, " BUSPHASE other\n");

		break;
	}

	//DEBUG(0, "%s: out\n", __FUNCTION__);
	return;	

timer_out:
	nsp_start_timer(tmpSC, data, 1000/102);
	return;
}

#ifdef PCMCIA_DEBUG
#include "nsp_debug.c"
#endif	/* DBG_SHOWCOMMAND */

/*----------------------------------------------------------------*/
/* look for ninja3 card and init if found			  */
/*----------------------------------------------------------------*/
static struct Scsi_Host *__nsp_detect(Scsi_Host_Template *sht)
{
	struct Scsi_Host *host;	/* registered host structure */
	nsp_hw_data *data = &nsp_data;

	DEBUG(0, "%s: this_id=%d\n", __FUNCTION__, sht->this_id);

	request_region(data->BaseAddress, data->NumAddress, "nsp_cs");
	host		  = scsi_register(sht, 0);
	if(host == NULL)
		return NULL;

	host->unique_id	  = data->BaseAddress;
	host->io_port	  = data->BaseAddress;
	host->n_io_port	  = data->NumAddress;
	host->irq	  = data->IrqNumber;
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(2,4,0))
	host->base        = data->MmioAddress; /* kernel 2.4 */
#else
	host->base        = (char *)(data->MmioAddress); /* 2.2 */
#endif

	spin_lock_init(&(data->Lock));

	snprintf(data->nspinfo,
		 sizeof(data->nspinfo),
		 "NinjaSCSI-3/32Bi Driver $Revision: 1.1 $ IO:0x%04lx-0x%04lx MMIO(virt addr):0x%04lx IRQ:%02d",
		 host->io_port, host->io_port + host->n_io_port - 1,
		 host->base,
		 host->irq);
	data->nspinfo[sizeof(data->nspinfo) - 1] = '\0';
	sht->name	  = data->nspinfo;

	DEBUG(0, "%s: end\n", __FUNCTION__);

	//MOD_INC_USE_COUNT;

	return host;
}

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2,5,0))
static int nsp_detect(Scsi_Host_Template *sht)
{
	return (__nsp_detect(sht) != NULL);
}

static int nsp_release(struct Scsi_Host *shpnt)
{
	//nsp_hw_data *data = &nsp_data;

	/* PCMCIA Card Service dose same things */
	//if (shpnt->irq) {
	//	free_irq(shpnt->irq, data);
	//}
	//if (shpnt->io_port) {
	//	release_region(shpnt->io_port, shpnt->n_io_port);
	//}

	//MOD_DEC_USE_COUNT;

	return 0;
}
#endif

/*----------------------------------------------------------------*/
/* return info string						  */
/*----------------------------------------------------------------*/
static const char *nsp_info(struct Scsi_Host *shpnt)
{
	nsp_hw_data *data = &nsp_data;

	return data->nspinfo;
}

#undef SPRINTF
#define SPRINTF(args...) \
        do { if(pos < buffer + length) pos += sprintf(pos, ## args); } while(0)
static int nsp_proc_info(struct Scsi_Host *host, char  *buffer,
			 char **start,
			 off_t  offset,
			 int    length,
			 int    inout)
{
	int id;
	char *pos = buffer;
	int thislength;
	int speed;
	unsigned long flags;
	nsp_hw_data *data = &nsp_data;

	if (inout) {
		return -EINVAL;
	}

	SPRINTF("NinjaSCSI status\n\n");
	SPRINTF("Driver version:        $Revision: 1.1 $\n");
	SPRINTF("SCSI host No.:         %d\n",          host->host_no);
	SPRINTF("IRQ:                   %d\n",          host->irq);
	SPRINTF("IO:                    0x%lx-0x%lx\n", host->io_port, host->io_port + host->n_io_port - 1);
	SPRINTF("MMIO(virtual address): 0x%lx\n",       host->base);
	SPRINTF("sg_tablesize:          %d\n",          host->sg_tablesize);

	SPRINTF("burst transfer mode:   ");
	switch (nsp_burst_mode) {
	case BURST_IO8:
		SPRINTF("io8");
		break;
	case BURST_IO32:
		SPRINTF("io32");
		break;
	case BURST_MEM32:
		SPRINTF("mem32");
		break;
	default:
		SPRINTF("???");
		break;
	}
	SPRINTF("\n");


	spin_lock_irqsave(&(data->Lock), flags);
	SPRINTF("CurrentSC:             0x%p\n\n",      data->CurrentSC);
	spin_unlock_irqrestore(&(data->Lock), flags);

	SPRINTF("SDTR status\n");
	for(id = 0; id < N_TARGET; id++) {

		SPRINTF("id %d: ", id);

		if (id == host->this_id) {
			SPRINTF("----- NinjaSCSI-3 host adapter\n");
			continue;
		}

		switch(data->Sync[id].SyncNegotiation) {
		case SYNC_OK:
			SPRINTF(" sync");
			break;
		case SYNC_NG:
			SPRINTF("async");
			break;
		case SYNC_NOT_YET:
			SPRINTF(" none");
			break;
		default:
			SPRINTF("?????");
			break;
		}

		if (data->Sync[id].SyncPeriod != 0) {
			speed = 1000000 / (data->Sync[id].SyncPeriod * 4);

			SPRINTF(" transfer %d.%dMB/s, offset %d",
				speed / 1000,
				speed % 1000,
				data->Sync[id].SyncOffset
				);
		}
		SPRINTF("\n");
	}

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


/*static int nsp_eh_strategy(struct Scsi_Host *Shost)
{
	return FAILED;
}*/

/*
static int nsp_eh_abort(Scsi_Cmnd *SCpnt)
{
	DEBUG(0, "%s: SCpnt=0x%p\n", __FUNCTION__, SCpnt);

	return nsp_eh_bus_reset(SCpnt);
}*/

/*
static int nsp_eh_device_reset(Scsi_Cmnd *SCpnt)
{
	DEBUG(0, "%s: SCpnt=0x%p\n", __FUNCTION__, SCpnt);

	return FAILED;
}*/

static int nsp_eh_bus_reset(Scsi_Cmnd *SCpnt)
{
	nsp_hw_data *data = &nsp_data;
	unsigned int base = SCpnt->device->host->io_port;
	int	     i;

	DEBUG(0, "%s: SCpnt=0x%p base=0x%x\n", __FUNCTION__, SCpnt, base);

	nsp_write(base, IRQCONTROL, IRQCONTROL_ALLMASK);

	nsp_index_write(base, SCSIBUSCTRL, SCSI_RST);
	mdelay(100); /* 100ms */
	nsp_index_write(base, SCSIBUSCTRL, 0);
	for(i = 0; i < 5; i++) {
		nsp_index_read(base, IRQPHASESENCE); /* dummy read */
	}

	nsp_write(base, IRQCONTROL, IRQCONTROL_ALLCLEAR);

	nsphw_init_sync(data);

	return SUCCESS;
}

static int nsp_eh_host_reset(Scsi_Cmnd *SCpnt)
{
	nsp_hw_data *data = &nsp_data;

	DEBUG(0, "%s:\n", __FUNCTION__);

	nsphw_init(data);

	return SUCCESS;
}


/**********************************************************************
  PCMCIA functions
**********************************************************************/

/*======================================================================
    nsp_cs_attach() creates an "instance" of the driver, allocating
    local data structures for one device.  The device is registered
    with Card Services.

    The dev_link structure is initialized, but we don't actually
    configure the card at this point -- we wait until we receive a
    card insertion event.
======================================================================*/
static dev_link_t *nsp_cs_attach(void)
{
	scsi_info_t  *info;
	client_reg_t  client_reg;
	dev_link_t   *link;
	int	      ret, i;

	DEBUG(0, "%s:\n", __FUNCTION__);

	/* Create new SCSI device */
	info = kmalloc(sizeof(*info), GFP_KERNEL);
	if (!info) { return NULL; }
	memset(info, 0, sizeof(*info));
	link = &info->link;
	link->priv = info;

	/* Initialize the dev_link_t structure */
	link->release.function	 = &nsp_cs_release;
	link->release.data	 = (u_long)link;

	/* The io structure describes IO port mapping */
	link->io.NumPorts1	 = 0x10;
	link->io.Attributes1	 = IO_DATA_PATH_WIDTH_AUTO;
	link->io.IOAddrLines	 = 10;	/* not used */

	/* Interrupt setup */
	link->irq.Attributes	 = IRQ_TYPE_EXCLUSIVE | IRQ_HANDLE_PRESENT;
	link->irq.IRQInfo1	 = IRQ_INFO2_VALID    | IRQ_LEVEL_ID;
	if (irq_list[0] == -1) {
		link->irq.IRQInfo2 = irq_mask;
	} else {
		for (i = 0; i < 4; i++) {
			link->irq.IRQInfo2 |= 1 << irq_list[i];
		}
	}

	/* IRQ の確保はここで PCMCIA の関数を用いて行なうので
	 * host->hostdata を irq.Instance に代入できない。
	 * host->hostdata が使えれば複数の NinjaSCSI が
	 * 使用できるのだが。
	 */
	link->irq.Handler	 = &nspintr;
	link->irq.Instance       = &nsp_data;
	link->irq.Attributes     |= (SA_SHIRQ | SA_SAMPLE_RANDOM);

	/* General socket configuration */
	link->conf.Attributes	 = CONF_ENABLE_IRQ;
	link->conf.Vcc		 = 50;
	link->conf.IntType	 = INT_MEMORY_AND_IO;
	link->conf.Present	 = PRESENT_OPTION;


	/* Register with Card Services */
	link->next               = dev_list;
	dev_list                 = link;
	client_reg.dev_info	 = &dev_info;
	client_reg.Attributes	 = INFO_IO_CLIENT | INFO_CARD_SHARE;
	client_reg.EventMask	 =
		CS_EVENT_CARD_INSERTION | CS_EVENT_CARD_REMOVAL |
		CS_EVENT_RESET_PHYSICAL | CS_EVENT_CARD_RESET	|
		CS_EVENT_PM_SUSPEND	| CS_EVENT_PM_RESUME	 ;
	client_reg.event_handler = &nsp_cs_event;
	client_reg.Version	 = 0x0210;
	client_reg.event_callback_args.client_data = link;
	ret = CardServices(RegisterClient, &link->handle, &client_reg);
	if (ret != CS_SUCCESS) {
		cs_error(link->handle, RegisterClient, ret);
		nsp_cs_detach(link);
		return NULL;
	}

	return link;
} /* nsp_cs_attach */


/*======================================================================
    This deletes a driver "instance".  The device is de-registered
    with Card Services.	 If it has been released, all local data
    structures are freed.  Otherwise, the structures will be freed
    when the device is released.
======================================================================*/
static void nsp_cs_detach(dev_link_t *link)
{
	dev_link_t **linkp;

	DEBUG(0, "%s(0x%p)\n", __FUNCTION__, link);
    
	/* Locate device structure */
	for (linkp = &dev_list; *linkp; linkp = &(*linkp)->next) {
		if (*linkp == link) {
			break;
		}
	}
	if (*linkp == NULL) {
		return;
	}

	del_timer(&link->release);
	if (link->state & DEV_CONFIG) {
		nsp_cs_release((u_long)link);
		if (link->state & DEV_STALE_CONFIG) {
			link->state |= DEV_STALE_LINK;
			return;
		}
	}

	/* Break the link with Card Services */
	if (link->handle) {
		CardServices(DeregisterClient, link->handle);
	}

	/* Unlink device structure, free bits */
	*linkp = link->next;
	kfree(link->priv);
	link->priv = NULL;

} /* nsp_cs_detach */


/*======================================================================
    nsp_cs_config() is scheduled to run after a CARD_INSERTION event
    is received, to configure the PCMCIA socket, and to make the
    ethernet device available to the system.
======================================================================*/
#define CS_CHECK(fn, args...) \
while ((last_ret=CardServices(last_fn=(fn),args))!=0) goto cs_failed
#define CFG_CHECK(fn, args...) \
if (CardServices(fn, args) != 0) goto next_entry
/*====================================================================*/

static void nsp_cs_config(dev_link_t *link)
{
	client_handle_t	  handle = link->handle;
	scsi_info_t	 *info	 = link->priv;
	tuple_t		  tuple;
	cisparse_t	  parse;
	int		  last_ret, last_fn;
	u_char		  tuple_data[64];
	config_info_t	  conf;
	win_req_t         req;
	memreq_t          map;
	cistpl_cftable_entry_t dflt = { 0 };

	struct Scsi_Host *host;
	nsp_hw_data      *data = &nsp_data;


	DEBUG(0, "%s: in\n", __FUNCTION__);

	tuple.DesiredTuple    = CISTPL_CONFIG;
	tuple.Attributes      = 0;
	tuple.TupleData	      = tuple_data;
	tuple.TupleDataMax    = sizeof(tuple_data);
	tuple.TupleOffset     = 0;
	CS_CHECK(GetFirstTuple, handle, &tuple);
	CS_CHECK(GetTupleData,	handle, &tuple);
	CS_CHECK(ParseTuple,	handle, &tuple, &parse);
	link->conf.ConfigBase = parse.config.base;
	link->conf.Present    = parse.config.rmask[0];

	/* Configure card */
	link->state	      |= DEV_CONFIG;

	/* Look up the current Vcc */
	CS_CHECK(GetConfigurationInfo, handle, &conf);
	link->conf.Vcc = conf.Vcc;

	tuple.DesiredTuple = CISTPL_CFTABLE_ENTRY;
	CS_CHECK(GetFirstTuple, handle, &tuple);
	while (1) {
		cistpl_cftable_entry_t *cfg = &(parse.cftable_entry);

		CFG_CHECK(GetTupleData, handle, &tuple);
		CFG_CHECK(ParseTuple,	handle, &tuple, &parse);

		if (cfg->flags & CISTPL_CFTABLE_DEFAULT) { dflt = *cfg; }
		if (cfg->index == 0) { goto next_entry; }
		link->conf.ConfigIndex = cfg->index;

		/* Does this card need audio output? */
		if (cfg->flags & CISTPL_CFTABLE_AUDIO) {
			link->conf.Attributes |= CONF_ENABLE_SPKR;
			link->conf.Status = CCSR_AUDIO_ENA;
		}

		/* Use power settings for Vcc and Vpp if present */
		/*  Note that the CIS values need to be rescaled */
		if (cfg->vcc.present & (1<<CISTPL_POWER_VNOM)) {
			if (conf.Vcc != cfg->vcc.param[CISTPL_POWER_VNOM]/10000) {
				goto next_entry;
			}
		} else if (dflt.vcc.present & (1<<CISTPL_POWER_VNOM)) {
			if (conf.Vcc != dflt.vcc.param[CISTPL_POWER_VNOM]/10000) {
				goto next_entry;
			}
		}

		if (cfg->vpp1.present & (1<<CISTPL_POWER_VNOM)) {
			link->conf.Vpp1 = link->conf.Vpp2 =
				cfg->vpp1.param[CISTPL_POWER_VNOM]/10000;
		} else if (dflt.vpp1.present & (1<<CISTPL_POWER_VNOM)) {
			link->conf.Vpp1 = link->conf.Vpp2 =
				dflt.vpp1.param[CISTPL_POWER_VNOM]/10000;
		}

		/* Do we need to allocate an interrupt? */
		if (cfg->irq.IRQInfo1 || dflt.irq.IRQInfo1) {
			link->conf.Attributes |= CONF_ENABLE_IRQ;
		}

		/* IO window settings */
		link->io.NumPorts1 = link->io.NumPorts2 = 0;
		if ((cfg->io.nwin > 0) || (dflt.io.nwin > 0)) {
			cistpl_io_t *io = (cfg->io.nwin) ? &cfg->io : &dflt.io;
			link->io.Attributes1 = IO_DATA_PATH_WIDTH_AUTO;
			if (!(io->flags & CISTPL_IO_8BIT))
				link->io.Attributes1 = IO_DATA_PATH_WIDTH_16;
			if (!(io->flags & CISTPL_IO_16BIT))
				link->io.Attributes1 = IO_DATA_PATH_WIDTH_8;
			link->io.IOAddrLines = io->flags & CISTPL_IO_LINES_MASK;
			link->io.BasePort1 = io->win[0].base;
			link->io.NumPorts1 = io->win[0].len;
			if (io->nwin > 1) {
				link->io.Attributes2 = link->io.Attributes1;
				link->io.BasePort2 = io->win[1].base;
				link->io.NumPorts2 = io->win[1].len;
			}
			/* This reserves IO space but doesn't actually enable it */
			CFG_CHECK(RequestIO, link->handle, &link->io);
		}

		if ((cfg->mem.nwin > 0) || (dflt.mem.nwin > 0)) {
			cistpl_mem_t *mem =
				(cfg->mem.nwin) ? &cfg->mem : &dflt.mem;
			req.Attributes = WIN_DATA_WIDTH_16|WIN_MEMORY_TYPE_CM;
			req.Attributes |= WIN_ENABLE;
			req.Base = mem->win[0].host_addr;
			req.Size = mem->win[0].len;
			if (req.Size < 0x1000)
				req.Size = 0x1000;
			req.AccessSpeed = 0;
			link->win = (window_handle_t)link->handle;
			CFG_CHECK(RequestWindow, &link->win, &req);
			map.Page = 0; map.CardOffset = mem->win[0].card_addr;
			CFG_CHECK(MapMemPage, link->win, &map);

			data->MmioAddress    = (u_long)ioremap_nocache(req.Base, req.Size);
		}
		/* If we got this far, we're cool! */
		break;

	next_entry:
		DEBUG(0, "%s: next\n", __FUNCTION__);

		if (link->io.NumPorts1)
			CardServices(ReleaseIO, link->handle, &link->io);
		CS_CHECK(GetNextTuple, handle, &tuple);
	}

	if (link->conf.Attributes & CONF_ENABLE_IRQ)
		CS_CHECK(RequestIRQ, link->handle, &link->irq);
	CS_CHECK(RequestConfiguration, handle, &link->conf);

	if (free_ports) {
		if (link->io.BasePort1)
			release_region(link->io.BasePort1, link->io.NumPorts1);
		if (link->io.BasePort2)
			release_region(link->io.BasePort2, link->io.NumPorts2);
	}

	/* Set port and IRQ */
	data->BaseAddress            = link->io.BasePort1;
	data->NumAddress             = link->io.NumPorts1;
	data->IrqNumber              = link->irq.AssignedIRQ;

	DEBUG(0, "%s: I/O[0x%x+0x%x] IRQ %d\n",
	      __FUNCTION__, data->BaseAddress, data->NumAddress, data->IrqNumber);

	if(nsphw_init(data) == FALSE) {
		goto cs_failed;
	}

	host = __nsp_detect(&nsp_driver_template);
	if (!host)
		goto cs_failed;

	sprintf(info->node.dev_name, "scsi%d", host->host_no);
	link->dev = &info->node;
	info->host = host;

	/* Finally, report what we've done */
	printk(KERN_INFO "nsp_cs: index 0x%02x: Vcc %d.%d",
	       link->conf.ConfigIndex,
	       link->conf.Vcc/10, link->conf.Vcc%10);
	if (link->conf.Vpp1) {
		printk(", Vpp %d.%d", link->conf.Vpp1/10, link->conf.Vpp1%10);
	}
	if (link->conf.Attributes & CONF_ENABLE_IRQ) {
		printk(", irq %d", link->irq.AssignedIRQ);
	}
	if (link->io.NumPorts1) {
		printk(", io 0x%04x-0x%04x", link->io.BasePort1,
		       link->io.BasePort1+link->io.NumPorts1-1);
	}
	if (link->io.NumPorts2)
		printk(" & 0x%04x-0x%04x", link->io.BasePort2,
		       link->io.BasePort2+link->io.NumPorts2-1);
	if (link->win)
		printk(", mem 0x%06lx-0x%06lx", req.Base,
		       req.Base+req.Size-1);
	printk("\n");

	scsi_add_host(host, NULL);
	link->state &= ~DEV_CONFIG_PENDING;
	return;

cs_failed:
	cs_error(link->handle, last_fn, last_ret);
	nsp_cs_release((u_long)link);
	return;

} /* nsp_cs_config */
#undef CS_CHECK
#undef CFG_CHECK

/*======================================================================
    After a card is removed, nsp_cs_release() will unregister the net
    device, and release the PCMCIA configuration.  If the device is
    still open, this will be postponed until it is closed.
======================================================================*/
static void nsp_cs_release(u_long arg)
{
	dev_link_t *link = (dev_link_t *)arg;
	scsi_info_t *info = link->priv;

	DEBUG(0, "%s(0x%p)\n", __FUNCTION__, link);

	/*
	 * If the device is currently in use, we won't release until it
	 * is actually closed.
	 */
	if (link->open) {
		DEBUG(1, "nsp_cs: release postponed, '%s' still open\n",
		      link->dev->dev_name);
		link->state |= DEV_STALE_CONFIG;
		return;
	}

	/* Unlink the device chain */
#if (LINUX_VERSION_CODE <= KERNEL_VERSION(2,5,2))
	scsi_unregister_module(MODULE_SCSI_HA, &nsp_driver_template);
#else
	scsi_remove_host(info->host);
	scsi_unregister(info->host);
#endif
	link->dev = NULL;

	if (link->win) {
		iounmap((void *)(nsp_data.MmioAddress));
		CardServices(ReleaseWindow, link->win);
	}
	CardServices(ReleaseConfiguration,  link->handle);
	if (link->io.NumPorts1) {
		CardServices(ReleaseIO,     link->handle, &link->io);
	}
	if (link->irq.AssignedIRQ) {
		CardServices(ReleaseIRQ,    link->handle, &link->irq);
	}
	link->state &= ~DEV_CONFIG;

	if (link->state & DEV_STALE_LINK) {
		nsp_cs_detach(link);
	}
} /* nsp_cs_release */

/*======================================================================

    The card status event handler.  Mostly, this schedules other
    stuff to run after an event is received.  A CARD_REMOVAL event
    also sets some flags to discourage the net drivers from trying
    to talk to the card any more.

    When a CARD_REMOVAL event is received, we immediately set a flag
    to block future accesses to this device.  All the functions that
    actually access the device should check this flag to make sure
    the card is still present.
    
======================================================================*/
static int nsp_cs_event(event_t		       event,
			int		       priority,
			event_callback_args_t *args)
{
	dev_link_t  *link = args->client_data;
	scsi_info_t *info = link->priv;
	Scsi_Cmnd tmp;

	DEBUG(1, "%s(0x%06x)\n", __FUNCTION__, event);

	switch (event) {
	case CS_EVENT_CARD_REMOVAL:
		DEBUG(0, " event: remove\n");
		link->state &= ~DEV_PRESENT;
		if (link->state & DEV_CONFIG) {
			((scsi_info_t *)link->priv)->stop = 1;
			mod_timer(&link->release, jiffies + HZ/20);
		}
		break;

	case CS_EVENT_CARD_INSERTION:
		DEBUG(0, " event: insert\n");
		link->state |= DEV_PRESENT | DEV_CONFIG_PENDING;
		nsp_cs_config(link);
		break;

	case CS_EVENT_PM_SUSPEND:
		link->state |= DEV_SUSPEND;
		/* Fall through... */
	case CS_EVENT_RESET_PHYSICAL:
		/* Mark the device as stopped, to block IO until later */
		info->stop = 1;
		if (link->state & DEV_CONFIG) {
			CardServices(ReleaseConfiguration, link->handle);
		}
		break;

	case CS_EVENT_PM_RESUME:
		link->state &= ~DEV_SUSPEND;
		/* Fall through... */
	case CS_EVENT_CARD_RESET:
		DEBUG(0, " event: reset\n");
		if (link->state & DEV_CONFIG) {
			CardServices(RequestConfiguration, link->handle, &link->conf);
		}
		info->stop = 0;

		tmp.device->host = info->host;
		nsp_eh_host_reset(&tmp);
		nsp_eh_bus_reset(&tmp);

		break;

	default:
		DEBUG(0, " event: unknown\n");
		break;
	}
	DEBUG(0, "%s: end\n", __FUNCTION__);
	return 0;
} /* nsp_cs_event */

static struct pcmcia_driver nsp_driver = {
	.owner		= THIS_MODULE,
	.drv		= {
		.name	= "nsp_cs",
	},
	.attach		= nsp_cs_attach,
	.detach		= nsp_cs_detach,
};

static int __init nsp_cs_init(void)
{
	return pcmcia_register_driver(&nsp_driver);
}


static void __exit nsp_cs_exit(void)
{
	pcmcia_unregister_driver(&nsp_driver);

	/* XXX: this really needs to move into generic code.. */
	while (dev_list != NULL) {
		if (dev_list->state & DEV_CONFIG) {
			nsp_cs_release((u_long)dev_list);
		}
		nsp_cs_detach(dev_list);
	}
}

module_init(nsp_cs_init)
module_exit(nsp_cs_exit)
