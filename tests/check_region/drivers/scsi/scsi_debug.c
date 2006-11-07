/* 
 *  linux/kernel/scsi_debug.c
 *
 *  Copyright (C) 1992  Eric Youngdale
 *  Simulate a host adapter with 2 disks attached.  Do a lot of checking
 *  to make sure that we are not getting blocks mixed up, and panic if
 *  anything out of the ordinary is seen.
 */

#include <linux/config.h>
#include <linux/module.h>

#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/timer.h>
#include <linux/types.h>
#include <linux/string.h>
#include <linux/genhd.h>
#include <linux/fs.h>
#include <linux/proc_fs.h>

#include <asm/system.h>
#include <asm/io.h>

#include <linux/blk.h>
#include "scsi.h"
#include "hosts.h"

#include "sd.h"

#include<linux/stat.h>

/* A few options that we want selected */

#define NR_HOSTS_PRESENT 1
#define NR_FAKE_DISKS   3
#define N_HEAD          255
#define N_SECTOR        63
#define N_CYLINDER      524
#define DISK_READONLY(TGT)      (0)
#define DISK_REMOVEABLE(TGT)    (1)
#define DEVICE_TYPE(TGT) (TGT == 2 ? TYPE_TAPE : TYPE_DISK);

/* Do not attempt to use a timer to simulate a real disk with latency */
/* Only use this in the actual kernel, not in the simulator. */
#define IMMEDIATE

/* Skip some consistency checking.  Good for benchmarking */
#define SPEEDY
/* Read return zeros. Undefine for benchmarking */
#define CLEAR

/* Number of real scsi disks that will be detected ahead of time */
static int NR_REAL = -1;

#define NR_BLK_DEV  12
#ifndef MAJOR_NR
#define MAJOR_NR 8
#endif
#define START_PARTITION 4

/* Time to wait before completing a command */
#define DISK_SPEED     (HZ/10)	/* 100ms */
#define CAPACITY (N_HEAD * N_SECTOR * N_CYLINDER)
#define SIZE(TGT) (TGT == 2 ? 2248 : 512)

static int starts[] =
{N_SECTOR,
 N_HEAD * N_SECTOR,		/* Single cylinder */
 N_HEAD * N_SECTOR * 4,
 CAPACITY, 0};
static int npart = 0;

#include "scsi_debug.h"
#ifdef DEBUG
#define DEB(x) x
#else
#define DEB(x)
#endif

#ifdef SPEEDY
#define VERIFY1_DEBUG(RW)
#define VERIFY_DEBUG(RW)
#else

#define VERIFY1_DEBUG(RW)                           \
    if (bufflen != 1024) {printk("%d", bufflen); panic("(1)Bad bufflen");};         \
    start = 0;                          \
    if ((MINOR(SCpnt->request.rq_dev) & 0xf) != 0) start = starts[(MINOR(SCpnt->request.rq_dev) & 0xf) - 1];        \
    if (bh){                            \
	if (bh->b_size != 1024) panic ("Wrong bh size");    \
	if ((bh->b_blocknr << 1) + start != block)          \
	{   printk("Wrong bh block# %d %d ",bh->b_blocknr, block);  \
	    panic ("Wrong bh block#"); \
	};  \
	if (bh->b_dev != SCpnt->request.rq_dev)  \
	    panic ("Bad bh target"); \
    };

#define VERIFY_DEBUG(RW)                            \
    if (bufflen != 1024 && (!SCpnt->use_sg)) {printk("%x %d\n ",bufflen, SCpnt->use_sg); panic("Bad bufflen");};    \
    start = 0;                          \
    if ((MINOR(SCpnt->request.rq_dev) & 0xf) > npart) panic ("Bad partition");    \
    if ((MINOR(SCpnt->request.rq_dev) & 0xf) != 0) start = starts[(MINOR(SCpnt->request.rq_dev) & 0xf) - 1];        \
    if (SCpnt->request.cmd != RW) panic ("Wrong  operation");       \
    if (SCpnt->request.sector + start != block) panic("Wrong block.");  \
    if (SCpnt->request.current_nr_sectors != 2 && (!SCpnt->use_sg)) panic ("Wrong # blocks");   \
    if (SCpnt->request.bh){                         \
	if (SCpnt->request.bh->b_size != 1024) panic ("Wrong bh size"); \
	if ((SCpnt->request.bh->b_blocknr << 1) + start != block)           \
	{   printk("Wrong bh block# %d %d ",SCpnt->request.bh->b_blocknr, block);  \
	    panic ("Wrong bh block#"); \
	};  \
	if (SCpnt->request.bh->b_dev != SCpnt->request.rq_dev) \
	    panic ("Bad bh target");\
    };
#endif

typedef void (*done_fct_t) (Scsi_Cmnd *);

static volatile done_fct_t do_done[SCSI_DEBUG_MAILBOXES] =
{NULL,};

struct Scsi_Host * SHpnt = NULL;

static void scsi_debug_send_self_command(struct Scsi_Host * shpnt);
static void scsi_debug_intr_handle(unsigned long);

static struct timer_list timeout[SCSI_DEBUG_MAILBOXES];

Scsi_Cmnd *SCint[SCSI_DEBUG_MAILBOXES] =
{NULL,};
static char SCrst[SCSI_DEBUG_MAILBOXES] =
{0,};

/*
 * Semaphore used to simulate bus lockups.
 */
static int scsi_debug_lockup = 0;

static char sense_buffer[128] =
{0,};

static void scsi_dump(Scsi_Cmnd * SCpnt, int flag)
{
	int i;
#if 0
	unsigned char *pnt;
#endif
	unsigned int *lpnt;
	struct scatterlist *sgpnt = NULL;
	printk("use_sg: %d", SCpnt->use_sg);
	if (SCpnt->use_sg) {
		sgpnt = (struct scatterlist *) SCpnt->buffer;
		for (i = 0; i < SCpnt->use_sg; i++) {
			lpnt = (int *) sgpnt[i].alt_address;
			printk(":%p %p %d\n", sgpnt[i].alt_address, sgpnt[i].address, sgpnt[i].length);
			if (lpnt)
				printk(" (Alt %x) ", lpnt[15]);
		};
	} else {
		printk("nosg: %p %p %d\n", SCpnt->request.buffer, SCpnt->buffer,
		       SCpnt->bufflen);
		lpnt = (int *) SCpnt->request.buffer;
		if (lpnt)
			printk(" (Alt %x) ", lpnt[15]);
	};
	lpnt = (unsigned int *) SCpnt;
	for (i = 0; i < sizeof(Scsi_Cmnd) / 4 + 1; i++) {
		if ((i & 7) == 0)
			printk("\n");
		printk("%x ", *lpnt++);
	};
	printk("\n");
	if (flag == 0)
		return;
	lpnt = (unsigned int *) sgpnt[0].alt_address;
	for (i = 0; i < sizeof(Scsi_Cmnd) / 4 + 1; i++) {
		if ((i & 7) == 0)
			printk("\n");
		printk("%x ", *lpnt++);
	};
#if 0
	printk("\n");
	lpnt = (unsigned int *) sgpnt[0].address;
	for (i = 0; i < sizeof(Scsi_Cmnd) / 4 + 1; i++) {
		if ((i & 7) == 0)
			printk("\n");
		printk("%x ", *lpnt++);
	};
	printk("\n");
#endif
	printk("DMA free %d sectors.\n", scsi_dma_free_sectors);
}

int scsi_debug_queuecommand(Scsi_Cmnd * SCpnt, void (*done) (Scsi_Cmnd *))
{
	unchar *cmd = (unchar *) SCpnt->cmnd;
	struct partition *p;
	int block;
	struct buffer_head *bh = NULL;
	unsigned char *buff;
	int nbytes, sgcount;
	int scsi_debug_errsts;
	struct scatterlist *sgpnt;
	int target = SCpnt->target;
	int bufflen = SCpnt->request_bufflen;
	unsigned long flags;
	int i;
	sgcount = 0;
	sgpnt = NULL;

#ifdef CONFIG_SMP
        /*
         * The io_request_lock *must* be held at this point.
         */
        if( io_request_lock.lock == 0 )
        {
                printk("Warning - io_request_lock is not held in queuecommand\n");
        }
#endif

	/*
	 * If we are being notified of the mid-level reposessing a command due to timeout,
	 * just return.
	 */
	if (done == NULL) {
		return 0;
	}
	DEB(if (target >= NR_FAKE_DISKS) {
	    SCpnt->result = DID_TIME_OUT << 16; done(SCpnt); return 0;
	    }
	);

	buff = (unsigned char *) SCpnt->request_buffer;

        /*
         * If a command comes for the ID of the host itself, just print
         * a silly message and return.
         */
        if( target == 7 ) {
                printk("How do you do!\n");
                SCpnt->result = 0;
                done(SCpnt);
                return 0;
        }

	if (target >= NR_FAKE_DISKS || SCpnt->lun != 0) {
		SCpnt->result = DID_NO_CONNECT << 16;
		done(SCpnt);
		return 0;
	}
	if (SCrst[target] != 0 && !scsi_debug_lockup) {
		SCrst[target] = 0;
		memset(SCpnt->sense_buffer, 0, sizeof(SCpnt->sense_buffer));
		SCpnt->sense_buffer[0] = 0x70;
		SCpnt->sense_buffer[2] = UNIT_ATTENTION;
		SCpnt->result = (CHECK_CONDITION << 1);
		done(SCpnt);
	}
	switch (*cmd) {
	case REQUEST_SENSE:
		SCSI_LOG_LLQUEUE(3, printk("Request sense...\n"));
#ifndef DEBUG
		{
			int i;
			printk("scsi_debug: Requesting sense buffer (%p %p %p %d):", SCpnt, buff, done, bufflen);
			for (i = 0; i < 12; i++)
				printk("%d ", sense_buffer[i]);
			printk("\n");
		};
#endif
		memset(buff, 0, bufflen);
		memcpy(buff, sense_buffer, bufflen);
		memset(sense_buffer, 0, sizeof(sense_buffer));
		SCpnt->result = 0;
		done(SCpnt);
		return 0;
	case START_STOP:
		SCSI_LOG_LLQUEUE(3, printk("START_STOP\n"));
		scsi_debug_errsts = 0;
		break;
	case ALLOW_MEDIUM_REMOVAL:
		if (cmd[4]) {
			SCSI_LOG_LLQUEUE(2, printk("Medium removal inhibited..."));
		} else {
			SCSI_LOG_LLQUEUE(2, printk("Medium removal enabled..."));
		}
		scsi_debug_errsts = 0;
		break;
	case INQUIRY:
		SCSI_LOG_LLQUEUE(3, printk("Inquiry...(%p %d)\n", buff, bufflen));
		memset(buff, 0, bufflen);
		buff[0] = DEVICE_TYPE(target);
		buff[1] = DISK_REMOVEABLE(target) ? 0x80 : 0;	/* Removable disk */
		buff[2] = 1;
		buff[4] = 33 - 5;
		memcpy(&buff[8], "Foo Inc", 7);
		memcpy(&buff[16], "XYZZY", 5);
		memcpy(&buff[32], "1", 1);
		scsi_debug_errsts = 0;
		break;
	case TEST_UNIT_READY:
		SCSI_LOG_LLQUEUE(3, printk("Test unit ready(%p %d)\n", buff, bufflen));
		if (buff)
			memset(buff, 0, bufflen);
		scsi_debug_errsts = 0;
		break;
	case READ_CAPACITY:
		SCSI_LOG_LLQUEUE(3, printk("Read Capacity\n"));
                SHpnt = SCpnt->host;
		if (NR_REAL < 0)
			NR_REAL = (MINOR(SCpnt->request.rq_dev) >> 4) & 0x0f;
		memset(buff, 0, bufflen);
		buff[0] = (CAPACITY >> 24);
		buff[1] = (CAPACITY >> 16) & 0xff;
		buff[2] = (CAPACITY >> 8) & 0xff;
		buff[3] = CAPACITY & 0xff;
		buff[4] = 0;
		buff[5] = 0;
		buff[6] = (SIZE(target) >> 8) & 0xff;	/* 512 byte sectors */
		buff[7] = SIZE(target) & 0xff;

		scsi_debug_errsts = 0;
		break;
	case READ_10:
	case READ_6:
#ifdef DEBUG
		printk("Read...");
#endif
		if ((*cmd) == READ_10)
			block = cmd[5] + (cmd[4] << 8) + (cmd[3] << 16) + (cmd[2] << 24);
		else
			block = cmd[3] + (cmd[2] << 8) + ((cmd[1] & 0x1f) << 16);
		VERIFY_DEBUG(READ);
#if defined(SCSI_SETUP_LATENCY) || defined(SCSI_DATARATE)
		{
			int delay = SCSI_SETUP_LATENCY;

			delay += SCpnt->request.nr_sectors * SCSI_DATARATE;
			if (delay)
				usleep(delay);
		};
#endif

#ifdef DEBUG
		printk("(r%d)", SCpnt->request.nr_sectors);
#endif
		nbytes = bufflen;
		if (SCpnt->use_sg) {
			sgcount = 0;
			sgpnt = (struct scatterlist *) buff;
			buff = sgpnt[sgcount].address;
			bufflen = sgpnt[sgcount].length;
			bh = SCpnt->request.bh;
		};
		scsi_debug_errsts = 0;
		do {
			VERIFY1_DEBUG(READ);
			/* For the speedy test, we do not even want to fill the buffer with anything */
#ifdef CLEAR
			memset(buff, 0, bufflen);
#endif
			/* If this is block 0, then we want to read the partition table for this
			 * device.  Let's make one up */
			if (block == 0) {
				int i;
				memset(buff, 0, bufflen);
				*((unsigned short *) (buff + 510)) = 0xAA55;
				p = (struct partition *) (buff + 0x1be);
				i = 0;
				while (starts[i + 1]) {
					int start_cyl, end_cyl;

					start_cyl = starts[i] / N_HEAD / N_SECTOR;
					end_cyl = (starts[i + 1] - 1) / N_HEAD / N_SECTOR;
					p->boot_ind = 0;

					p->head = (i == 0 ? 1 : 0);
					p->sector = 1 | ((start_cyl >> 8) << 6);
					p->cyl = (start_cyl & 0xff);

					p->end_head = N_HEAD - 1;
					p->end_sector = N_SECTOR | ((end_cyl >> 8) << 6);
					p->end_cyl = (end_cyl & 0xff);

					p->start_sect = starts[i];
					p->nr_sects = starts[i + 1] - starts[i];
					p->sys_ind = 0x81;	/* Linux partition */
					p++;
					i++;
				};
				if (!npart)
					npart = i;
				scsi_debug_errsts = 0;
				break;
			};
#ifdef DEBUG
			if (SCpnt->use_sg)
				printk("Block %x (%d %d)\n", block, SCpnt->request.nr_sectors,
				       SCpnt->request.current_nr_sectors);
#endif

#if 0
			/* Simulate a disk change */
			if (block == 0xfff0) {
				sense_buffer[0] = 0x70;
				sense_buffer[2] = UNIT_ATTENTION;
				starts[0] += 10;
				starts[1] += 10;
				starts[2] += 10;

#ifdef DEBUG
				{
					int i;
					printk("scsi_debug: Filling sense buffer:");
					for (i = 0; i < 12; i++)
						printk("%d ", sense_buffer[i]);
					printk("\n");
				};
#endif
				scsi_debug_errsts = (COMMAND_COMPLETE << 8) | (CHECK_CONDITION << 1);
				break;
			}	/* End phony disk change code */
#endif

#ifdef CLEAR
			memcpy(buff, &target, sizeof(target));
			memcpy(buff + sizeof(target), cmd, 24);
			memcpy(buff + 60, &block, sizeof(block));
			memcpy(buff + 64, SCpnt, sizeof(Scsi_Cmnd));
#endif
			nbytes -= bufflen;
			if (SCpnt->use_sg) {
#ifdef CLEAR
				memcpy(buff + 128, bh, sizeof(struct buffer_head));
#endif
				block += bufflen >> 9;
				bh = bh->b_reqnext;
				sgcount++;
				if (nbytes) {
					if (!bh)
						panic("Too few blocks for linked request.");
					buff = sgpnt[sgcount].address;
					bufflen = sgpnt[sgcount].length;
				};
			}
		} while (nbytes);

		SCpnt->result = 0;
		(done) (SCpnt);
		return 0;

		if (SCpnt->use_sg && !scsi_debug_errsts)
			if (bh)
				scsi_dump(SCpnt, 0);
		break;
	case WRITE_10:
	case WRITE_6:
#ifdef DEBUG
		printk("Write\n");
#endif
		if ((*cmd) == WRITE_10)
			block = cmd[5] + (cmd[4] << 8) + (cmd[3] << 16) + (cmd[2] << 24);
		else
			block = cmd[3] + (cmd[2] << 8) + ((cmd[1] & 0x1f) << 16);
		VERIFY_DEBUG(WRITE);
		/*      printk("(w%d)",SCpnt->request.nr_sectors); */
		if (SCpnt->use_sg) {
			if ((bufflen >> 9) != SCpnt->request.nr_sectors)
				panic("Trying to write wrong number of blocks\n");
			sgpnt = (struct scatterlist *) buff;
			buff = sgpnt[sgcount].address;
		};
#if 0
		if (block != *((unsigned long *) (buff + 60))) {
			printk("%x %x :", block, *((unsigned long *) (buff + 60)));
			scsi_dump(SCpnt, 1);
			panic("Bad block written.\n");
		};
#endif
		scsi_debug_errsts = 0;
		break;
	case MODE_SENSE:
		/*
		 * Used to detect write protected status.
		 */
		scsi_debug_errsts = 0;
		memset(buff, 0, 6);
		break;
	default:
		SCSI_LOG_LLQUEUE(3, printk("Unknown command %d\n", *cmd));
		SCpnt->result = DID_NO_CONNECT << 16;
		done(SCpnt);
		return 0;
	};

	save_flags(flags);
	cli();
	for (i = 0; i < SCSI_DEBUG_MAILBOXES; i++) {
		if (timeout[i].function == NULL)
			break;
	};

	/*
	 * If all of the slots are full, just return 1.  The new error handling scheme
	 * allows this, and the mid-level should queue things.
	 */
	if (i >= SCSI_DEBUG_MAILBOXES || timeout[i].function != 0) {
		SCSI_LOG_LLQUEUE(1, printk("Command rejected - host busy\n"));
		restore_flags(flags);
		return 1;
	}
	SCSI_LOG_LLQUEUE(1, printk("Command accepted - slot %d\n", i));

#ifdef IMMEDIATE
	if (!scsi_debug_lockup) {
		SCpnt->result = scsi_debug_errsts;
		SCint[i] = SCpnt;
		do_done[i] = done;
		scsi_debug_intr_handle(i);	/* No timer - do this one right away */
	}
	restore_flags(flags);
#else

	SCpnt->result = scsi_debug_errsts;
	timeout[i].function = scsi_debug_intr_handle;
	timeout[i].data = i;
	timeout[i].expires = jiffies + DISK_SPEED;
	SCint[i] = SCpnt;
	do_done[i] = done;

	restore_flags(flags);
	add_timer(&timeout[i]);
	if (!done)
		panic("scsi_debug_queuecommand: done can't be NULL\n");

#if 0
	printk("Sending command (%d %x %d %d)...", i, done, timeout[i].expires, jiffies);
#endif
#endif

	return 0;
}

static void scsi_debug_send_self_command(struct Scsi_Host * shpnt)
{
	static unsigned char cmd[6] =
	{TEST_UNIT_READY, 0, 0, 0, 0, 0};

        Scsi_Request  * scp;
        Scsi_Device   * sdev;
        
        printk("Allocating host dev\n");
        sdev = scsi_get_host_dev(shpnt);
        if(sdev==NULL)
        {
        	printk("Out of memory.\n");
        	return;
        }
        
        printk("Got %p. Allocating command block\n", sdev);
        scp  = scsi_allocate_request(sdev);
        printk("Got %p\n", scp);
        
        if(scp==NULL)
        {
        	printk("Out of memory.\n");
        	goto bail;
        }

        scp->sr_cmd_len = 6;
        scp->sr_use_sg = 0;
        
        printk("Sending command\n");
        scsi_wait_req (scp, (void *) cmd, (void *) NULL,
                       0, 100, 3);
        
        printk("Releasing command\n");
        scsi_release_request(scp);
bail:
	printk("Freeing device\n");
        scsi_free_host_dev(sdev);
}

/* A "high" level interrupt handler.  This should be called once per jiffy
 * to simulate a regular scsi disk.  We use a timer to do this. */

static void scsi_debug_intr_handle(unsigned long indx)
{
	Scsi_Cmnd *SCtmp;
	void (*my_done) (Scsi_Cmnd *);
#ifdef DEBUG
	int to;
#endif

#if 0
	del_timer(&timeout[indx]);
#endif

	SCtmp = (Scsi_Cmnd *) SCint[indx];
	my_done = do_done[indx];
	do_done[indx] = NULL;
	timeout[indx].function = NULL;
	SCint[indx] = NULL;

	if (!my_done) {
		printk("scsi_debug_intr_handle: Unexpected interrupt\n");
		return;
	}
#ifdef DEBUG
	printk("In intr_handle...");
	printk("...done %d %x %d %d\n", i, my_done, to, jiffies);
	printk("In intr_handle: %d %x %x\n", i, SCtmp, my_done);
#endif

	my_done(SCtmp);
#ifdef DEBUG
	printk("Called done.\n");
#endif
}


int scsi_debug_detect(Scsi_Host_Template * tpnt)
{
	int i;

	for (i = 0; i < NR_HOSTS_PRESENT; i++) {
		tpnt->proc_name = "scsi_debug";	/* Huh? In the loop??? */
		scsi_register(tpnt, 0);
	}
	return NR_HOSTS_PRESENT;
}

int scsi_debug_abort(Scsi_Cmnd * SCpnt)
{
#if 0
	int j;
	void (*my_done) (Scsi_Cmnd *);
	unsigned long flags;
#endif

	DEB(printk("scsi_debug_abort\n"));
#if 0
	SCpnt->result = SCpnt->abort_reason << 16;
	for (j = 0; j < SCSI_DEBUG_MAILBOXES; j++) {
		if (SCpnt == SCint[j]) {
			my_done = do_done[j];
			my_done(SCpnt);
			save_flags(flags);
			cli();
			timeout[j] = 0;
			SCint[j] = NULL;
			do_done[j] = NULL;
			restore_flags(flags);
		};
	};
#endif
	return SCSI_ABORT_SNOOZE;
}

int scsi_debug_biosparam(Disk * disk, kdev_t dev, int *info)
{
	int size = disk->capacity;
	info[0] = N_HEAD;
	info[1] = N_SECTOR;
	info[2] = N_CYLINDER;
	if (info[2] >= 1024)
		info[2] = 1024;
	return 0;
}

int scsi_debug_reset(Scsi_Cmnd * SCpnt, unsigned int why)
{
	int i;
	unsigned long flags;

	void (*my_done) (Scsi_Cmnd *);
	printk("Bus unlocked by reset - %d\n", why);
	scsi_debug_lockup = 0;
	DEB(printk("scsi_debug_reset called\n"));
	for (i = 0; i < SCSI_DEBUG_MAILBOXES; i++) {
		if (SCint[i] == NULL)
			continue;
		SCint[i]->result = DID_RESET << 16;
		my_done = do_done[i];
		my_done(SCint[i]);
		save_flags(flags);
		cli();
		SCint[i] = NULL;
		do_done[i] = NULL;
		timeout[i].function = NULL;
		restore_flags(flags);
	}
	return SCSI_RESET_SUCCESS;
}

const char *scsi_debug_info(void)
{
	static char buffer[] = " ";	/* looks nicer without anything here */
	return buffer;
}

/* scsi_debug_proc_info
 * Used if the driver currently has no own support for /proc/scsi
 */
int scsi_debug_proc_info(char *buffer, char **start, off_t offset,
			 int length, int inode, int inout)
{
	int len, pos, begin;
	int orig_length;

	orig_length = length;

	if (inout == 1) {
		/* First check for the Signature */
		if (length >= 10 && strncmp(buffer, "scsi_debug", 10) == 0) {
			buffer += 11;
			length -= 11;

			if (buffer[length - 1] == '\n') {
				buffer[length - 1] = '\0';
				length--;
			}
			/*
			 * OK, we are getting some kind of command.  Figure out
			 * what we are supposed to do here.  Simulate bus lockups
			 * to test our reset capability.
			 */
			if (length == 4 && strncmp(buffer, "test", length) == 0) {
                                printk("Testing send self command %p\n", SHpnt);
                                scsi_debug_send_self_command(SHpnt);
                                return orig_length;
                        }
			if (length == 6 && strncmp(buffer, "lockup", length) == 0) {
				scsi_debug_lockup = 1;
				return orig_length;
			}
			if (length == 6 && strncmp(buffer, "unlock", length) == 0) {
				scsi_debug_lockup = 0;
				return orig_length;
			}
			printk("Unknown command:%s (%d)\n", buffer, length);
		} else
			printk("Wrong Signature:%10s\n", (char *) buffer);

		return -EINVAL;

	}
	begin = 0;
	pos = len = sprintf(buffer,
	"This driver is not a real scsi driver, but it plays one on TV.\n"
	 "It is very handy for debugging specific problems because you\n"
			 "can simulate a variety of error conditions\n");
	if (pos < offset) {
		len = 0;
		begin = pos;
	}
	*start = buffer + (offset - begin);	/* Start of wanted data */
	len -= (offset - begin);
	if (len > length)
		len = length;

	return (len);
}

#ifdef CONFIG_USER_DEBUG
/*
 * This is a hack for the user space emulator.  It allows us to
 * "insert" arbitrary numbers of additional drivers.
 */
void *scsi_debug_get_handle(void)
{
	static Scsi_Host_Template driver_copy = SCSI_DEBUG;
	void *rtn;
	rtn = kmalloc(sizeof(driver_copy), GFP_ATOMIC);
	if(rtn==NULL)
		return NULL;
	memcpy(rtn, (void *) &driver_copy, sizeof(driver_copy));
	return rtn;
}
#endif

/* Eventually this will go into an include file, but this will be later */
static Scsi_Host_Template driver_template = SCSI_DEBUG;

#include "scsi_module.c"

/*
 * Overrides for Emacs so that we almost follow Linus's tabbing style.
 * Emacs will notice this stuff at the end of the file and automatically
 * adjust the settings for this buffer only.  This must remain at the end
 * of the file.
 * ---------------------------------------------------------------------------
 * Local variables:
 * c-indent-level: 4
 * c-brace-imaginary-offset: 0
 * c-brace-offset: -4
 * c-argdecl-indent: 4
 * c-label-offset: -4
 * c-continued-statement-offset: 4
 * c-continued-brace-offset: 0
 * indent-tabs-mode: nil
 * tab-width: 8
 * End:
 */
