/*
 * linux/drivers/scsi/ide-scsi.c	Version 0.9		Jul   4, 1999
 *
 * Copyright (C) 1996 - 1999 Gadi Oxman <gadio@netvision.net.il>
 */

/*
 * Emulation of a SCSI host adapter for IDE ATAPI devices.
 *
 * With this driver, one can use the Linux SCSI drivers instead of the
 * native IDE ATAPI drivers.
 *
 * Ver 0.1   Dec  3 96   Initial version.
 * Ver 0.2   Jan 26 97   Fixed bug in cleanup_module() and added emulation
 *                        of MODE_SENSE_6/MODE_SELECT_6 for cdroms. Thanks
 *                        to Janos Farkas for pointing this out.
 *                       Avoid using bitfields in structures for m68k.
 *                       Added Scatter/Gather and DMA support.
 * Ver 0.4   Dec  7 97   Add support for ATAPI PD/CD drives.
 *                       Use variable timeout for each command.
 * Ver 0.5   Jan  2 98   Fix previous PD/CD support.
 *                       Allow disabling of SCSI-6 to SCSI-10 transformation.
 * Ver 0.6   Jan 27 98   Allow disabling of SCSI command translation layer
 *                        for access through /dev/sg.
 *                       Fix MODE_SENSE_6/MODE_SELECT_6/INQUIRY translation.
 * Ver 0.7   Dec 04 98   Ignore commands where lun != 0 to avoid multiple
 *                        detection of devices with CONFIG_SCSI_MULTI_LUN
 * Ver 0.8   Feb 05 99   Optical media need translation too. Reverse 0.7.
 * Ver 0.9   Jul 04 99   Fix a bug in SG_SET_TRANSFORM.
 */

#define IDESCSI_VERSION "0.9"

#include <linux/module.h>
#include <linux/types.h>
#include <linux/string.h>
#include <linux/kernel.h>
#include <linux/mm.h>
#include <linux/ioport.h>
#include <linux/blkdev.h>
#include <linux/errno.h>
#include <linux/hdreg.h>
#include <linux/malloc.h>
#include <linux/ide.h>

#include <asm/io.h>
#include <asm/bitops.h>
#include <asm/uaccess.h>

#include "scsi.h"
#include "hosts.h"
#include "sd.h"
#include "ide-scsi.h"
#include <scsi/sg.h>

#define IDESCSI_DEBUG_LOG		0

typedef struct idescsi_pc_s {
	u8 c[12];				/* Actual packet bytes */
	int request_transfer;			/* Bytes to transfer */
	int actually_transferred;		/* Bytes actually transferred */
	int buffer_size;			/* Size of our data buffer */
	struct request *rq;			/* The corresponding request */
	byte *buffer;				/* Data buffer */
	byte *current_position;			/* Pointer into the above buffer */
	struct scatterlist *sg;			/* Scatter gather table */
	int b_count;				/* Bytes transferred from current entry */
	Scsi_Cmnd *scsi_cmd;			/* SCSI command */
	void (*done)(Scsi_Cmnd *);		/* Scsi completion routine */
	unsigned long flags;			/* Status/Action flags */
	unsigned long timeout;			/* Command timeout */
} idescsi_pc_t;

/*
 *	Packet command status bits.
 */
#define PC_DMA_IN_PROGRESS		0	/* 1 while DMA in progress */
#define PC_WRITING			1	/* Data direction */
#define PC_TRANSFORM			2	/* transform SCSI commands */

/*
 *	SCSI command transformation layer
 */
#define IDESCSI_TRANSFORM		0	/* Enable/Disable transformation */
#define IDESCSI_SG_TRANSFORM		1	/* /dev/sg transformation */

/*
 *	Log flags
 */
#define IDESCSI_LOG_CMD			0	/* Log SCSI commands */

typedef struct {
	ide_drive_t *drive;
	idescsi_pc_t *pc;			/* Current packet command */
	unsigned long flags;			/* Status/Action flags */
	unsigned long transform;		/* SCSI cmd translation layer */
	unsigned long log;			/* log flags */
} idescsi_scsi_t;

/*
 *	Per ATAPI device status bits.
 */
#define IDESCSI_DRQ_INTERRUPT		0	/* DRQ interrupt device */

/*
 *	ide-scsi requests.
 */
#define IDESCSI_PC_RQ			90

/*
 *	Bits of the interrupt reason register.
 */
#define IDESCSI_IREASON_COD	0x1		/* Information transferred is command */
#define IDESCSI_IREASON_IO	0x2		/* The device requests us to read */

static void idescsi_discard_data (ide_drive_t *drive, unsigned int bcount)
{
	while (bcount--)
		IN_BYTE (IDE_DATA_REG);
}

static void idescsi_output_zeros (ide_drive_t *drive, unsigned int bcount)
{
	while (bcount--)
		OUT_BYTE (0, IDE_DATA_REG);
}

/*
 *	PIO data transfer routines using the scatter gather table.
 */
static void idescsi_input_buffers (ide_drive_t *drive, idescsi_pc_t *pc, unsigned int bcount)
{
	int count;

	while (bcount) {
		if (pc->sg - (struct scatterlist *) pc->scsi_cmd->request_buffer > pc->scsi_cmd->use_sg) {
			printk (KERN_ERR "ide-scsi: scatter gather table too small, discarding data\n");
			idescsi_discard_data (drive, bcount);
			return;
		}
		count = IDE_MIN (pc->sg->length - pc->b_count, bcount);
		atapi_input_bytes (drive, pc->sg->address + pc->b_count, count);
		bcount -= count; pc->b_count += count;
		if (pc->b_count == pc->sg->length) {
			pc->sg++;
			pc->b_count = 0;
		}
	}
}

static void idescsi_output_buffers (ide_drive_t *drive, idescsi_pc_t *pc, unsigned int bcount)
{
	int count;

	while (bcount) {
		if (pc->sg - (struct scatterlist *) pc->scsi_cmd->request_buffer > pc->scsi_cmd->use_sg) {
			printk (KERN_ERR "ide-scsi: scatter gather table too small, padding with zeros\n");
			idescsi_output_zeros (drive, bcount);
			return;
		}
		count = IDE_MIN (pc->sg->length - pc->b_count, bcount);
		atapi_output_bytes (drive, pc->sg->address + pc->b_count, count);
		bcount -= count; pc->b_count += count;
		if (pc->b_count == pc->sg->length) {
			pc->sg++;
			pc->b_count = 0;
		}
	}
}

/*
 *	Most of the SCSI commands are supported directly by ATAPI devices.
 *	idescsi_transform_pc handles the few exceptions.
 */
static inline void idescsi_transform_pc1 (ide_drive_t *drive, idescsi_pc_t *pc)
{
	u8 *c = pc->c, *scsi_buf = pc->buffer, *sc = pc->scsi_cmd->cmnd;
	char *atapi_buf;

	if (!test_bit(PC_TRANSFORM, &pc->flags))
		return;
	if (drive->media == ide_cdrom || drive->media == ide_optical) {
		if (c[0] == READ_6 || c[0] == WRITE_6) {
			c[8] = c[4];		c[5] = c[3];		c[4] = c[2];
			c[3] = c[1] & 0x1f;	c[2] = 0;		c[1] &= 0xe0;
			c[0] += (READ_10 - READ_6);
		}
		if (c[0] == MODE_SENSE || c[0] == MODE_SELECT) {
			if (!scsi_buf)
				return;
			if ((atapi_buf = kmalloc(pc->buffer_size + 4, GFP_ATOMIC)) == NULL)
				return;
			memset(atapi_buf, 0, pc->buffer_size + 4);
			memset (c, 0, 12);
			c[0] = sc[0] | 0x40;	c[1] = sc[1];		c[2] = sc[2];
			c[8] = sc[4] + 4;	c[9] = sc[5];
			if (sc[4] + 4 > 255)
				c[7] = sc[4] + 4 - 255;
			if (c[0] == MODE_SELECT_10) {
				atapi_buf[1] = scsi_buf[0];	/* Mode data length */
				atapi_buf[2] = scsi_buf[1];	/* Medium type */
				atapi_buf[3] = scsi_buf[2];	/* Device specific parameter */
				atapi_buf[7] = scsi_buf[3];	/* Block descriptor length */
				memcpy(atapi_buf + 8, scsi_buf + 4, pc->buffer_size - 4);
			}
			pc->buffer = atapi_buf;
			pc->request_transfer += 4;
			pc->buffer_size += 4;
		}
	}
}

static inline void idescsi_transform_pc2 (ide_drive_t *drive, idescsi_pc_t *pc)
{
	u8 *atapi_buf = pc->buffer;
	u8 *sc = pc->scsi_cmd->cmnd;
	u8 *scsi_buf = pc->scsi_cmd->request_buffer;

	if (!test_bit(PC_TRANSFORM, &pc->flags))
		return;
	if (drive->media == ide_cdrom || drive->media == ide_optical) {
		if (pc->c[0] == MODE_SENSE_10 && sc[0] == MODE_SENSE) {
			scsi_buf[0] = atapi_buf[1];		/* Mode data length */
			scsi_buf[1] = atapi_buf[2];		/* Medium type */
			scsi_buf[2] = atapi_buf[3];		/* Device specific parameter */
			scsi_buf[3] = atapi_buf[7];		/* Block descriptor length */
			memcpy(scsi_buf + 4, atapi_buf + 8, pc->request_transfer - 8);
		}
		if (pc->c[0] == INQUIRY) {
			scsi_buf[2] |= 2;			/* ansi_revision */
			scsi_buf[3] = (scsi_buf[3] & 0xf0) | 2;	/* response data format */
		}
	}
	if (atapi_buf && atapi_buf != scsi_buf)
		kfree(atapi_buf);
}

static inline void idescsi_free_bh (struct buffer_head *bh)
{
	struct buffer_head *bhp;

	while (bh) {
		bhp = bh;
		bh = bh->b_reqnext;
		kfree (bhp);
	}
}

static void hexdump(u8 *x, int len)
{
	int i;

	printk("[ ");
	for (i = 0; i < len; i++)
		printk("%x ", x[i]);
	printk("]\n");
}

static void idescsi_end_request (byte uptodate, ide_hwgroup_t *hwgroup)
{
	ide_drive_t *drive = hwgroup->drive;
	idescsi_scsi_t *scsi = drive->driver_data;
	struct request *rq = hwgroup->rq;
	idescsi_pc_t *pc = (idescsi_pc_t *) rq->buffer;
	int log = test_bit(IDESCSI_LOG_CMD, &scsi->log);
	u8 *scsi_buf;
	unsigned long flags;

	if (rq->cmd != IDESCSI_PC_RQ) {
		ide_end_request (uptodate, hwgroup);
		return;
	}
	ide_end_drive_cmd (drive, 0, 0);
	if (rq->errors >= ERROR_MAX) {
		pc->scsi_cmd->result = DID_ERROR << 16;
		if (log)
			printk ("ide-scsi: %s: I/O error for %lu\n", drive->name, pc->scsi_cmd->serial_number);
	} else if (rq->errors) {
		pc->scsi_cmd->result = (CHECK_CONDITION << 1) | (DID_OK << 16);
		if (log)
			printk ("ide-scsi: %s: check condition for %lu\n", drive->name, pc->scsi_cmd->serial_number);
	} else {
		pc->scsi_cmd->result = DID_OK << 16;
		idescsi_transform_pc2 (drive, pc);
		if (log) {
			printk ("ide-scsi: %s: suc %lu", drive->name, pc->scsi_cmd->serial_number);
			if (!test_bit(PC_WRITING, &pc->flags) && pc->actually_transferred && pc->actually_transferred <= 1024 && pc->buffer) {
				printk(", rst = ");
				scsi_buf = pc->scsi_cmd->request_buffer;
				hexdump(scsi_buf, IDE_MIN(16, pc->scsi_cmd->request_bufflen));
			} else printk("\n");
		}
	}
	spin_lock_irqsave(&io_request_lock,flags);	
	pc->done(pc->scsi_cmd);
	spin_unlock_irqrestore(&io_request_lock,flags);
	idescsi_free_bh (rq->bh);
	kfree(pc); kfree(rq);
	scsi->pc = NULL;
}

static inline unsigned long get_timeout(idescsi_pc_t *pc)
{
	return IDE_MAX(WAIT_CMD, pc->timeout - jiffies);
}

/*
 *	Our interrupt handler.
 */
static ide_startstop_t idescsi_pc_intr (ide_drive_t *drive)
{
	idescsi_scsi_t *scsi = drive->driver_data;
	byte status, ireason;
	int bcount;
	idescsi_pc_t *pc=scsi->pc;
	struct request *rq = pc->rq;
	unsigned int temp;

#if IDESCSI_DEBUG_LOG
	printk (KERN_INFO "ide-scsi: Reached idescsi_pc_intr interrupt handler\n");
#endif /* IDESCSI_DEBUG_LOG */

	if (test_and_clear_bit (PC_DMA_IN_PROGRESS, &pc->flags)) {
#if IDESCSI_DEBUG_LOG
		printk ("ide-scsi: %s: DMA complete\n", drive->name);
#endif /* IDESCSI_DEBUG_LOG */
		pc->actually_transferred=pc->request_transfer;
		(void) (HWIF(drive)->dmaproc(ide_dma_end, drive));
	}

	status = GET_STAT();						/* Clear the interrupt */

	if ((status & DRQ_STAT) == 0) {					/* No more interrupts */
		if (test_bit(IDESCSI_LOG_CMD, &scsi->log))
			printk (KERN_INFO "Packet command completed, %d bytes transferred\n", pc->actually_transferred);
		ide__sti();
		if (status & ERR_STAT)
			rq->errors++;
		idescsi_end_request (1, HWGROUP(drive));
		return ide_stopped;
	}
	bcount = IN_BYTE (IDE_BCOUNTH_REG) << 8 | IN_BYTE (IDE_BCOUNTL_REG);
	ireason = IN_BYTE (IDE_IREASON_REG);

	if (ireason & IDESCSI_IREASON_COD) {
		printk (KERN_ERR "ide-scsi: CoD != 0 in idescsi_pc_intr\n");
		return ide_do_reset (drive);
	}
	if (ireason & IDESCSI_IREASON_IO) {
		temp = pc->actually_transferred + bcount;
		if ( temp > pc->request_transfer) {
			if (temp > pc->buffer_size) {
				printk (KERN_ERR "ide-scsi: The scsi wants to send us more data than expected - discarding data\n");
				temp = pc->buffer_size - pc->actually_transferred;
				if (temp) {
					clear_bit(PC_WRITING, &pc->flags);
					if (pc->sg)
						idescsi_input_buffers(drive, pc, temp);
					else
						atapi_input_bytes(drive, pc->current_position, temp);
					printk(KERN_ERR "ide-scsi: transferred %d of %d bytes\n", temp, bcount);
				}
				pc->actually_transferred += temp;
				pc->current_position += temp;
				idescsi_discard_data (drive,bcount - temp);
				ide_set_handler(drive, &idescsi_pc_intr, get_timeout(pc), NULL);
				return ide_started;
			}
#if IDESCSI_DEBUG_LOG
			printk (KERN_NOTICE "ide-scsi: The scsi wants to send us more data than expected - allowing transfer\n");
#endif /* IDESCSI_DEBUG_LOG */
		}
	}
	if (ireason & IDESCSI_IREASON_IO) {
		clear_bit(PC_WRITING, &pc->flags);
		if (pc->sg)
			idescsi_input_buffers (drive, pc, bcount);
		else
			atapi_input_bytes (drive,pc->current_position,bcount);
	} else {
		set_bit(PC_WRITING, &pc->flags);
		if (pc->sg)
			idescsi_output_buffers (drive, pc, bcount);
		else
			atapi_output_bytes (drive,pc->current_position,bcount);
	}
	pc->actually_transferred+=bcount;				/* Update the current position */
	pc->current_position+=bcount;

	ide_set_handler(drive, &idescsi_pc_intr, get_timeout(pc), NULL);	/* And set the interrupt handler again */
	return ide_started;
}

static ide_startstop_t idescsi_transfer_pc (ide_drive_t *drive)
{
	idescsi_scsi_t *scsi = drive->driver_data;
	idescsi_pc_t *pc = scsi->pc;
	byte ireason;
	ide_startstop_t startstop;

	if (ide_wait_stat (&startstop,drive,DRQ_STAT,BUSY_STAT,WAIT_READY)) {
		printk (KERN_ERR "ide-scsi: Strange, packet command initiated yet DRQ isn't asserted\n");
		return startstop;
	}
	ireason = IN_BYTE (IDE_IREASON_REG);
	if ((ireason & (IDESCSI_IREASON_IO | IDESCSI_IREASON_COD)) != IDESCSI_IREASON_COD) {
		printk (KERN_ERR "ide-scsi: (IO,CoD) != (0,1) while issuing a packet command\n");
		return ide_do_reset (drive);
	}
	ide_set_handler(drive, &idescsi_pc_intr, get_timeout(pc), NULL);	/* Set the interrupt routine */
	atapi_output_bytes (drive, scsi->pc->c, 12);			/* Send the actual packet */
	return ide_started;
}

/*
 *	Issue a packet command
 */
static ide_startstop_t idescsi_issue_pc (ide_drive_t *drive, idescsi_pc_t *pc)
{
	idescsi_scsi_t *scsi = drive->driver_data;
	int bcount;
	struct request *rq = pc->rq;
	int dma_ok = 0;

	scsi->pc=pc;							/* Set the current packet command */
	pc->actually_transferred=0;					/* We haven't transferred any data yet */
	pc->current_position=pc->buffer;
	bcount = IDE_MIN (pc->request_transfer, 63 * 1024);		/* Request to transfer the entire buffer at once */

	if (drive->using_dma && rq->bh)
		dma_ok=!HWIF(drive)->dmaproc(test_bit (PC_WRITING, &pc->flags) ? ide_dma_write : ide_dma_read, drive);

	SELECT_DRIVE(HWIF(drive), drive);
	if (IDE_CONTROL_REG)
		OUT_BYTE (drive->ctl,IDE_CONTROL_REG);
	OUT_BYTE (dma_ok,IDE_FEATURE_REG);
	OUT_BYTE (bcount >> 8,IDE_BCOUNTH_REG);
	OUT_BYTE (bcount & 0xff,IDE_BCOUNTL_REG);

	if (dma_ok) {
		set_bit (PC_DMA_IN_PROGRESS, &pc->flags);
		(void) (HWIF(drive)->dmaproc(ide_dma_begin, drive));
	}
	if (test_bit (IDESCSI_DRQ_INTERRUPT, &scsi->flags)) {
		ide_set_handler (drive, &idescsi_transfer_pc, get_timeout(pc), NULL);
		OUT_BYTE (WIN_PACKETCMD, IDE_COMMAND_REG);		/* Issue the packet command */
		return ide_started;
	} else {
		OUT_BYTE (WIN_PACKETCMD, IDE_COMMAND_REG);
		return idescsi_transfer_pc (drive);
	}
}

/*
 *	idescsi_do_request is our request handling function.
 */
static ide_startstop_t idescsi_do_request (ide_drive_t *drive, struct request *rq, unsigned long block)
{
#if IDESCSI_DEBUG_LOG
	printk (KERN_INFO "rq_status: %d, rq_dev: %u, cmd: %d, errors: %d\n",rq->rq_status,(unsigned int) rq->rq_dev,rq->cmd,rq->errors);
	printk (KERN_INFO "sector: %ld, nr_sectors: %ld, current_nr_sectors: %ld\n",rq->sector,rq->nr_sectors,rq->current_nr_sectors);
#endif /* IDESCSI_DEBUG_LOG */

	if (rq->cmd == IDESCSI_PC_RQ) {
		return idescsi_issue_pc (drive, (idescsi_pc_t *) rq->buffer);
	}
	printk (KERN_ERR "ide-scsi: %s: unsupported command in request queue (%x)\n", drive->name, rq->cmd);
	idescsi_end_request (0,HWGROUP (drive));
	return ide_stopped;
}

static int idescsi_open (struct inode *inode, struct file *filp, ide_drive_t *drive)
{
	MOD_INC_USE_COUNT;
	return 0;
}

static void idescsi_ide_release (struct inode *inode, struct file *filp, ide_drive_t *drive)
{
	MOD_DEC_USE_COUNT;
}

static ide_drive_t *idescsi_drives[MAX_HWIFS * MAX_DRIVES];
static int idescsi_initialized = 0;

static void idescsi_add_settings(ide_drive_t *drive)
{
	idescsi_scsi_t *scsi = drive->driver_data;

/*
 *			drive	setting name	read/write	ioctl	ioctl		data type	min	max	mul_factor	div_factor	data pointer		set function
 */
	ide_add_setting(drive,	"bios_cyl",	SETTING_RW,	-1,	-1,		TYPE_INT,	0,	1023,	1,		1,		&drive->bios_cyl,	NULL);
	ide_add_setting(drive,	"bios_head",	SETTING_RW,	-1,	-1,		TYPE_BYTE,	0,	255,	1,		1,		&drive->bios_head,	NULL);
	ide_add_setting(drive,	"bios_sect",	SETTING_RW,	-1,	-1,		TYPE_BYTE,	0,	63,	1,		1,		&drive->bios_sect,	NULL);
	ide_add_setting(drive,	"transform",	SETTING_RW,	-1,	-1,		TYPE_INT,	0,	3,	1,		1,		&scsi->transform,	NULL);
	ide_add_setting(drive,	"log",		SETTING_RW,	-1,	-1,		TYPE_INT,	0,	1,	1,		1,		&scsi->log,		NULL);
}

/*
 *	Driver initialization.
 */
static void idescsi_setup (ide_drive_t *drive, idescsi_scsi_t *scsi, int id)
{
	DRIVER(drive)->busy++;
	idescsi_drives[id] = drive;
	drive->driver_data = scsi;
	drive->ready_stat = 0;
	memset (scsi, 0, sizeof (idescsi_scsi_t));
	scsi->drive = drive;
	if (drive->id && (drive->id->config & 0x0060) == 0x20)
		set_bit (IDESCSI_DRQ_INTERRUPT, &scsi->flags);
	set_bit(IDESCSI_TRANSFORM, &scsi->transform);
	clear_bit(IDESCSI_SG_TRANSFORM, &scsi->transform);
#if IDESCSI_DEBUG_LOG
	set_bit(IDESCSI_LOG_CMD, &scsi->log);
#endif /* IDESCSI_DEBUG_LOG */
	idescsi_add_settings(drive);
}

static int idescsi_cleanup (ide_drive_t *drive)
{
	idescsi_scsi_t *scsi = drive->driver_data;

	if (ide_unregister_subdriver (drive))
		return 1;
	drive->driver_data = NULL;
	kfree (scsi);
	return 0;
}

/*
 *	IDE subdriver functions, registered with ide.c
 */
static ide_driver_t idescsi_driver = {
	"ide-scsi",		/* name */
	IDESCSI_VERSION,	/* version */
	ide_scsi,		/* media */
	0,			/* busy */
	1,			/* supports_dma */
	0,			/* supports_dsc_overlap */
	idescsi_cleanup,	/* cleanup */
	idescsi_do_request,	/* do_request */
	idescsi_end_request,	/* end_request */
	NULL,			/* ioctl */
	idescsi_open,		/* open */
	idescsi_ide_release,	/* release */
	NULL,			/* media_change */
	NULL,			/* revalidate */
	NULL,			/* pre_reset */
	NULL,			/* capacity */
	NULL,			/* special */
	NULL			/* proc */
};

int idescsi_init (void);
static ide_module_t idescsi_module = {
	IDE_DRIVER_MODULE,
	idescsi_init,
	&idescsi_driver,
	NULL
};

/*
 *	idescsi_init will register the driver for each scsi.
 */
int idescsi_init (void)
{
	ide_drive_t *drive;
	idescsi_scsi_t *scsi;
	byte media[] = {TYPE_DISK, TYPE_TAPE, TYPE_PROCESSOR, TYPE_WORM, TYPE_ROM, TYPE_SCANNER, TYPE_MOD, 255};
	int i, failed, id;

	if (idescsi_initialized)
		return 0;
	idescsi_initialized = 1;
	for (i = 0; i < MAX_HWIFS * MAX_DRIVES; i++)
		idescsi_drives[i] = NULL;
	MOD_INC_USE_COUNT;
	for (i = 0; media[i] != 255; i++) {
		failed = 0;
		while ((drive = ide_scan_devices (media[i], idescsi_driver.name, NULL, failed++)) != NULL) {

			if ((scsi = (idescsi_scsi_t *) kmalloc (sizeof (idescsi_scsi_t), GFP_KERNEL)) == NULL) {
				printk (KERN_ERR "ide-scsi: %s: Can't allocate a scsi structure\n", drive->name);
				continue;
			}
			if (ide_register_subdriver (drive, &idescsi_driver, IDE_SUBDRIVER_VERSION)) {
				printk (KERN_ERR "ide-scsi: %s: Failed to register the driver with ide.c\n", drive->name);
				kfree (scsi);
				continue;
			}
			for (id = 0; id < MAX_HWIFS * MAX_DRIVES && idescsi_drives[id]; id++);
			idescsi_setup (drive, scsi, id);
			failed--;
		}
	}
	ide_register_module(&idescsi_module);
	MOD_DEC_USE_COUNT;
	return 0;
}

int idescsi_detect (Scsi_Host_Template *host_template)
{
	struct Scsi_Host *host;
	int id;
	int last_lun = 0;

	host_template->proc_name = "ide-scsi";
	host = scsi_register(host_template, 0);
	if(host == NULL)
		return 0;
		
	for (id = 0; id < MAX_HWIFS * MAX_DRIVES && idescsi_drives[id]; id++)
		last_lun = IDE_MAX(last_lun, idescsi_drives[id]->last_lun);
	host->max_id = id;
	host->max_lun = last_lun + 1;
	host->can_queue = host->cmd_per_lun * id;
	return 1;
}

int idescsi_release (struct Scsi_Host *host)
{
	ide_drive_t *drive;
	int id;

	for (id = 0; id < MAX_HWIFS * MAX_DRIVES; id++) {
		drive = idescsi_drives[id];
		if (drive)
			DRIVER(drive)->busy--;
	}
	return 0;
}

const char *idescsi_info (struct Scsi_Host *host)
{
	return "SCSI host adapter emulation for IDE ATAPI devices";
}

int idescsi_ioctl (Scsi_Device *dev, int cmd, void *arg)
{
	ide_drive_t *drive = idescsi_drives[dev->id];
	idescsi_scsi_t *scsi = drive->driver_data;

	if (cmd == SG_SET_TRANSFORM) {
		if (arg)
			set_bit(IDESCSI_SG_TRANSFORM, &scsi->transform);
		else
			clear_bit(IDESCSI_SG_TRANSFORM, &scsi->transform);
		return 0;
	} else if (cmd == SG_GET_TRANSFORM)
		return put_user(test_bit(IDESCSI_SG_TRANSFORM, &scsi->transform), (int *) arg);
	return -EINVAL;
}

static inline struct buffer_head *idescsi_kmalloc_bh (int count)
{
	struct buffer_head *bh, *bhp, *first_bh;

	if ((first_bh = bhp = bh = kmalloc (sizeof(struct buffer_head), GFP_ATOMIC)) == NULL)
		goto abort;
	memset (bh, 0, sizeof (struct buffer_head));
	bh->b_reqnext = NULL;
	while (--count) {
		if ((bh = kmalloc (sizeof(struct buffer_head), GFP_ATOMIC)) == NULL)
			goto abort;
		memset (bh, 0, sizeof (struct buffer_head));
		bhp->b_reqnext = bh;
		bhp = bh;
		bh->b_reqnext = NULL;
	}
	return first_bh;
abort:
	idescsi_free_bh (first_bh);
	return NULL;
}

static inline int idescsi_set_direction (idescsi_pc_t *pc)
{
	switch (pc->c[0]) {
		case READ_6: case READ_10: case READ_12:
			clear_bit (PC_WRITING, &pc->flags);
			return 0;
		case WRITE_6: case WRITE_10: case WRITE_12:
			set_bit (PC_WRITING, &pc->flags);
			return 0;
		default:
			return 1;
	}
}

static inline struct buffer_head *idescsi_dma_bh (ide_drive_t *drive, idescsi_pc_t *pc)
{
	struct buffer_head *bh = NULL, *first_bh = NULL;
	int segments = pc->scsi_cmd->use_sg;
	struct scatterlist *sg = pc->scsi_cmd->request_buffer;

	if (!drive->using_dma || !pc->request_transfer || pc->request_transfer % 1024)
		return NULL;
	if (idescsi_set_direction(pc))
		return NULL;
	if (segments) {
		if ((first_bh = bh = idescsi_kmalloc_bh (segments)) == NULL)
			return NULL;
#if IDESCSI_DEBUG_LOG
		printk ("ide-scsi: %s: building DMA table, %d segments, %dkB total\n", drive->name, segments, pc->request_transfer >> 10);
#endif /* IDESCSI_DEBUG_LOG */
		while (segments--) {
			bh->b_data = sg->address;
			bh->b_size = sg->length;
			bh = bh->b_reqnext;
			sg++;
		}
	} else {
		if ((first_bh = bh = idescsi_kmalloc_bh (1)) == NULL)
			return NULL;
#if IDESCSI_DEBUG_LOG
		printk ("ide-scsi: %s: building DMA table for a single buffer (%dkB)\n", drive->name, pc->request_transfer >> 10);
#endif /* IDESCSI_DEBUG_LOG */
		bh->b_data = pc->scsi_cmd->request_buffer;
		bh->b_size = pc->request_transfer;
	}
	return first_bh;
}

static inline int should_transform(ide_drive_t *drive, Scsi_Cmnd *cmd)
{
	idescsi_scsi_t *scsi = drive->driver_data;

	if (MAJOR(cmd->request.rq_dev) == SCSI_GENERIC_MAJOR)
		return test_bit(IDESCSI_SG_TRANSFORM, &scsi->transform);
	return test_bit(IDESCSI_TRANSFORM, &scsi->transform);
}

int idescsi_queue (Scsi_Cmnd *cmd, void (*done)(Scsi_Cmnd *))
{
	ide_drive_t *drive = idescsi_drives[cmd->target];
	idescsi_scsi_t *scsi;
	struct request *rq = NULL;
	idescsi_pc_t *pc = NULL;

	if (!drive) {
		printk (KERN_ERR "ide-scsi: drive id %d not present\n", cmd->target);
		goto abort;
	}
	scsi = drive->driver_data;
	pc = kmalloc (sizeof (idescsi_pc_t), GFP_ATOMIC);
	rq = kmalloc (sizeof (struct request), GFP_ATOMIC);
	if (rq == NULL || pc == NULL) {
		printk (KERN_ERR "ide-scsi: %s: out of memory\n", drive->name);
		goto abort;
	}

	memset (pc->c, 0, 12);
	pc->flags = 0;
	pc->rq = rq;
	memcpy (pc->c, cmd->cmnd, cmd->cmd_len);
	if (cmd->use_sg) {
		pc->buffer = NULL;
		pc->sg = cmd->request_buffer;
	} else {
		pc->buffer = cmd->request_buffer;
		pc->sg = NULL;
	}
	pc->b_count = 0;
	pc->request_transfer = pc->buffer_size = cmd->request_bufflen;
	pc->scsi_cmd = cmd;
	pc->done = done;
	pc->timeout = jiffies + cmd->timeout_per_command;

	if (should_transform(drive, cmd))
		set_bit(PC_TRANSFORM, &pc->flags);
	idescsi_transform_pc1 (drive, pc);

	if (test_bit(IDESCSI_LOG_CMD, &scsi->log)) {
		printk ("ide-scsi: %s: que %lu, cmd = ", drive->name, cmd->serial_number);
		hexdump(cmd->cmnd, cmd->cmd_len);
		if (memcmp(pc->c, cmd->cmnd, cmd->cmd_len)) {
			printk ("ide-scsi: %s: que %lu, tsl = ", drive->name, cmd->serial_number);
			hexdump(pc->c, 12);
		}
	}

	ide_init_drive_cmd (rq);
	rq->buffer = (char *) pc;
	rq->bh = idescsi_dma_bh (drive, pc);
	rq->cmd = IDESCSI_PC_RQ;
	spin_unlock(&io_request_lock);
	(void) ide_do_drive_cmd (drive, rq, ide_end);
	spin_lock_irq(&io_request_lock);
	return 0;
abort:
	if (pc) kfree (pc);
	if (rq) kfree (rq);
	cmd->result = DID_ERROR << 16;
	done(cmd);
	return 0;
}

int idescsi_abort (Scsi_Cmnd *cmd)
{
	return SCSI_ABORT_SNOOZE;
}

int idescsi_reset (Scsi_Cmnd *cmd, unsigned int resetflags)
{
	return SCSI_RESET_SUCCESS;
}

int idescsi_bios (Disk *disk, kdev_t dev, int *parm)
{
	ide_drive_t *drive = idescsi_drives[disk->device->id];

	if (drive->bios_cyl && drive->bios_head && drive->bios_sect) {
		parm[0] = drive->bios_head;
		parm[1] = drive->bios_sect;
		parm[2] = drive->bios_cyl;
	}
	return 0;
}

static Scsi_Host_Template idescsi_template = IDESCSI;

static int __init init_idescsi_module(void)
{
	idescsi_init();
	idescsi_template.module = THIS_MODULE;
	scsi_register_module (MODULE_SCSI_HA, &idescsi_template);
	return 0;
}

static void __exit exit_idescsi_module(void)
{
	ide_drive_t *drive;
	byte media[] = {TYPE_DISK, TYPE_TAPE, TYPE_PROCESSOR, TYPE_WORM, TYPE_ROM, TYPE_SCANNER, TYPE_MOD, 255};
	int i, failed;

	scsi_unregister_module (MODULE_SCSI_HA, &idescsi_template);
	for (i = 0; media[i] != 255; i++) {
		failed = 0;
		while ((drive = ide_scan_devices (media[i], idescsi_driver.name, &idescsi_driver, failed)) != NULL)
			if (idescsi_cleanup (drive)) {
				printk ("%s: exit_idescsi_module() called while still busy\n", drive->name);
				failed++;
			}
	}
	ide_unregister_module(&idescsi_module);
}

module_init(init_idescsi_module);
module_exit(exit_idescsi_module);
