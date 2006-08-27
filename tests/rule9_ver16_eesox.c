/*
 *  linux/drivers/acorn/scsi/eesox.c
 *
 *  Copyright (C) 1997-2003 Russell King
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 *  This driver is based on experimentation.  Hence, it may have made
 *  assumptions about the particular card that I have available, and
 *  may not be reliable!
 *
 *  Changelog:
 *   01-10-1997	RMK		Created, READONLY version
 *   15-02-1998	RMK		READ/WRITE version
 *				added DMA support and hardware definitions
 *   14-03-1998	RMK		Updated DMA support
 *				Added terminator control
 *   15-04-1998	RMK		Only do PIO if FAS216 will allow it.
 *   27-06-1998	RMK		Changed asm/delay.h to linux/delay.h
 *   02-04-2000	RMK	0.0.3	Fixed NO_IRQ/NO_DMA problem, updated for new
 *				error handling code.
 */
#include <linux/module.h>
#include <linux/blk.h>
#include <linux/kernel.h>
#include <linux/string.h>
#include <linux/ioport.h>
#include <linux/sched.h>
#include <linux/proc_fs.h>
#include <linux/delay.h>
#include <linux/interrupt.h>
#include <linux/init.h>
#include <linux/dma-mapping.h>

#include <asm/io.h>
#include <asm/irq.h>
#include <asm/dma.h>
#include <asm/ecard.h>
#include <asm/pgtable.h>

#include "../scsi.h"
#include "../hosts.h"
#include "fas216.h"
#include "scsi.h"

#include <scsi/scsicam.h>

#define EESOX_FAS216_OFFSET	0x3000
#define EESOX_FAS216_SHIFT	5

#define EESOX_DMASTAT		0x2800
#define EESOX_STAT_INTR		0x01
#define EESOX_STAT_DMA		0x02

#define EESOX_CONTROL		0x2800
#define EESOX_INTR_ENABLE	0x04
#define EESOX_TERM_ENABLE	0x02
#define EESOX_RESET		0x01

#define EESOX_DMADATA		0x3800

#define VERSION "1.10 (17/01/2003 2.5.59)"

/*
 * Use term=0,1,0,0,0 to turn terminators on/off
 */
static int term[MAX_ECARDS] = { 1, 1, 1, 1, 1, 1, 1, 1 };

#define NR_SG	256

struct eesoxscsi_info {
	FAS216_Info		info;
	struct expansion_card	*ec;

	void			*ctl_port;
	unsigned int		control;
	struct scatterlist	sg[NR_SG];	/* Scatter DMA list	*/
};

/* Prototype: void eesoxscsi_irqenable(ec, irqnr)
 * Purpose  : Enable interrupts on EESOX SCSI card
 * Params   : ec    - expansion card structure
 *          : irqnr - interrupt number
 */
static void
eesoxscsi_irqenable(struct expansion_card *ec, int irqnr)
{
	struct eesoxscsi_info *info = (struct eesoxscsi_info *)ec->irq_data;

	info->control |= EESOX_INTR_ENABLE;

	writeb(info->control, info->ctl_port);
}

/* Prototype: void eesoxscsi_irqdisable(ec, irqnr)
 * Purpose  : Disable interrupts on EESOX SCSI card
 * Params   : ec    - expansion card structure
 *          : irqnr - interrupt number
 */
static void
eesoxscsi_irqdisable(struct expansion_card *ec, int irqnr)
{
	struct eesoxscsi_info *info = (struct eesoxscsi_info *)ec->irq_data;

	info->control &= ~EESOX_INTR_ENABLE;

	writeb(info->control, info->ctl_port);
}

static const expansioncard_ops_t eesoxscsi_ops = {
	.irqenable	= eesoxscsi_irqenable,
	.irqdisable	= eesoxscsi_irqdisable,
};

/* Prototype: void eesoxscsi_terminator_ctl(*host, on_off)
 * Purpose  : Turn the EESOX SCSI terminators on or off
 * Params   : host   - card to turn on/off
 *          : on_off - !0 to turn on, 0 to turn off
 */
static void
eesoxscsi_terminator_ctl(struct Scsi_Host *host, int on_off)
{
	struct eesoxscsi_info *info = (struct eesoxscsi_info *)host->hostdata;
	unsigned long flags;

	spin_lock_irqsave(host->host_lock, flags);
	if (on_off)
		info->control |= EESOX_TERM_ENABLE;
	else
		info->control &= ~EESOX_TERM_ENABLE;

	writeb(info->control, info->ctl_port);
	spin_unlock_irqrestore(host->host_lock, flags);
}

/* Prototype: void eesoxscsi_intr(irq, *dev_id, *regs)
 * Purpose  : handle interrupts from EESOX SCSI card
 * Params   : irq    - interrupt number
 *	      dev_id - user-defined (Scsi_Host structure)
 *	      regs   - processor registers at interrupt
 */
static irqreturn_t
eesoxscsi_intr(int irq, void *dev_id, struct pt_regs *regs)
{
	struct eesoxscsi_info *info = dev_id;

	return fas216_intr(&info->info);
}

/* Prototype: fasdmatype_t eesoxscsi_dma_setup(host, SCpnt, direction, min_type)
 * Purpose  : initialises DMA/PIO
 * Params   : host      - host
 *	      SCpnt     - command
 *	      direction - DMA on to/off of card
 *	      min_type  - minimum DMA support that we must have for this transfer
 * Returns  : type of transfer to be performed
 */
static fasdmatype_t
eesoxscsi_dma_setup(struct Scsi_Host *host, Scsi_Pointer *SCp,
		       fasdmadir_t direction, fasdmatype_t min_type)
{
	struct eesoxscsi_info *info = (struct eesoxscsi_info *)host->hostdata;
	struct device *dev = scsi_get_device(host);
	int dmach = host->dma_channel;

	if (dmach != NO_DMA &&
	    (min_type == fasdma_real_all || SCp->this_residual >= 512)) {
		int bufs, map_dir, dma_dir;

		bufs = copy_SCp_to_sg(&info->sg[0], SCp, NR_SG);

		if (direction == DMA_OUT)
			map_dir = DMA_TO_DEVICE,
			dma_dir = DMA_MODE_WRITE;
		else
			map_dir = DMA_FROM_DEVICE,
			dma_dir = DMA_MODE_READ;

		dma_map_sg(dev, info->sg, bufs + 1, map_dir);

		disable_dma(dmach);
		set_dma_sg(dmach, info->sg, bufs + 1);
		set_dma_mode(dmach, dma_dir);
		enable_dma(dmach);
		return fasdma_real_all;
	}
	/*
	 * We don't do DMA, we only do slow PIO
	 *
	 * Some day, we will do Pseudo DMA
	 */
	return fasdma_pseudo;
}

static void eesoxscsi_buffer_in(void *buf, int length, void *base)
{
	const void *reg_fas = base + EESOX_FAS216_OFFSET;
	const void *reg_dmastat = base + EESOX_DMASTAT;
	const void *reg_dmadata = base + EESOX_DMADATA;
	const register unsigned long mask = 0xffff;

	do {
		unsigned int status;

		/*
		 * Interrupt request?
		 */
		status = readb(reg_fas + (REG_STAT << EESOX_FAS216_SHIFT));
		if (status & STAT_INT)
			break;

		/*
		 * DMA request active?
		 */
		status = readb(reg_dmastat);
		if (!(status & EESOX_STAT_DMA))
			continue;

		/*
		 * Get number of bytes in FIFO
		 */
		status = readb(reg_fas + (REG_CFIS << EESOX_FAS216_SHIFT)) & CFIS_CF;
		if (status > 16)
			status = 16;
		if (status > length)
			status = length;

		/*
		 * Align buffer.
		 */
		if (((u32)buf) & 2 && status >= 2) {
			*((u16 *)buf)++ = readl(reg_dmadata);
			status -= 2;
			length -= 2;
		}

		if (status >= 8) {
			unsigned long l1, l2;

			l1 = readl(reg_dmadata) & mask;
			l1 |= readl(reg_dmadata) << 16;
			l2 = readl(reg_dmadata) & mask;
			l2 |= readl(reg_dmadata) << 16;
			*((u32 *)buf)++ = l1;
			*((u32 *)buf)++ = l2;
			length -= 8;
			continue;
		}

		if (status >= 4) {
			unsigned long l1;

			l1 = readl(reg_dmadata) & mask;
			l1 |= readl(reg_dmadata) << 16;

			*((u32 *)buf)++ = l1;
			length -= 4;
			continue;
		}

		if (status >= 2) {
			*((u16 *)buf)++ = readl(reg_dmadata);
			length -= 2;
		}
	} while (length);
}

static void eesoxscsi_buffer_out(void *buf, int length, void *base)
{
	const void *reg_fas = base + EESOX_FAS216_OFFSET;
	const void *reg_dmastat = base + EESOX_DMASTAT;
	const void *reg_dmadata = base + EESOX_DMADATA;

	do {
		unsigned int status;

		/*
		 * Interrupt request?
		 */
		status = readb(reg_fas + (REG_STAT << EESOX_FAS216_SHIFT));
		if (status & STAT_INT)
			break;

		/*
		 * DMA request active?
		 */
		status = readb(reg_dmastat);
		if (!(status & EESOX_STAT_DMA))
			continue;

		/*
		 * Get number of bytes in FIFO
		 */
		status = readb(reg_fas + (REG_CFIS << EESOX_FAS216_SHIFT)) & CFIS_CF;
		if (status > 16)
			status = 16;
		status = 16 - status;
		if (status > length)
			status = length;
		status &= ~1;

		/*
		 * Align buffer.
		 */
		if (((u32)buf) & 2 && status >= 2) {
			writel(*((u16 *)buf)++ << 16, reg_dmadata);
			status -= 2;
			length -= 2;
		}

		if (status >= 8) {
			unsigned long l1, l2;

			l1 = *((u32 *)buf)++;
			l2 = *((u32 *)buf)++;

			writel(l1 << 16, reg_dmadata);
			writel(l1, reg_dmadata);
			writel(l2 << 16, reg_dmadata);
			writel(l2, reg_dmadata);
			length -= 8;
			continue;
		}

		if (status >= 4) {
			unsigned long l1;

			l1 = *((u32 *)buf)++;

			writel(l1 << 16, reg_dmadata);
			writel(l1, reg_dmadata);
			length -= 4;
			continue;
		}

		if (status >= 2) {
			writel(*((u16 *)buf)++ << 16, reg_dmadata);
			length -= 2;
		}
	} while (length);
}

static void
eesoxscsi_dma_pseudo(struct Scsi_Host *host, Scsi_Pointer *SCp,
		     fasdmadir_t dir, int transfer_size)
{
	void *base = (void *)host->base;
	if (dir == DMA_IN) {
		eesoxscsi_buffer_in(SCp->ptr, SCp->this_residual, base);
	} else {
		eesoxscsi_buffer_out(SCp->ptr, SCp->this_residual, base);
	}
}

/* Prototype: int eesoxscsi_dma_stop(host, SCpnt)
 * Purpose  : stops DMA/PIO
 * Params   : host  - host
 *	      SCpnt - command
 */
static void
eesoxscsi_dma_stop(struct Scsi_Host *host, Scsi_Pointer *SCp)
{
	if (host->dma_channel != NO_DMA)
		disable_dma(host->dma_channel);
}

/* Prototype: const char *eesoxscsi_info(struct Scsi_Host * host)
 * Purpose  : returns a descriptive string about this interface,
 * Params   : host - driver host structure to return info for.
 * Returns  : pointer to a static buffer containing null terminated string.
 */
const char *eesoxscsi_info(struct Scsi_Host *host)
{
	struct eesoxscsi_info *info = (struct eesoxscsi_info *)host->hostdata;
	static char string[150];

	sprintf(string, "%s (%s) in slot %d v%s terminators o%s",
		host->hostt->name, info->info.scsi.type, info->ec->slot_no,
		VERSION, info->control & EESOX_TERM_ENABLE ? "n" : "ff");

	return string;
}

/* Prototype: int eesoxscsi_set_proc_info(struct Scsi_Host *host, char *buffer, int length)
 * Purpose  : Set a driver specific function
 * Params   : host   - host to setup
 *          : buffer - buffer containing string describing operation
 *          : length - length of string
 * Returns  : -EINVAL, or 0
 */
static int
eesoxscsi_set_proc_info(struct Scsi_Host *host, char *buffer, int length)
{
	int ret = length;

	if (length >= 9 && strncmp(buffer, "EESOXSCSI", 9) == 0) {
		buffer += 9;
		length -= 9;

		if (length >= 5 && strncmp(buffer, "term=", 5) == 0) {
			if (buffer[5] == '1')
				eesoxscsi_terminator_ctl(host, 1);
			else if (buffer[5] == '0')
				eesoxscsi_terminator_ctl(host, 0);
			else
				ret = -EINVAL;
		} else
			ret = -EINVAL;
	} else
		ret = -EINVAL;

	return ret;
}

/* Prototype: int eesoxscsi_proc_info(char *buffer, char **start, off_t offset,
 *				      int length, int host_no, int inout)
 * Purpose  : Return information about the driver to a user process accessing
 *	      the /proc filesystem.
 * Params   : buffer - a buffer to write information to
 *	      start  - a pointer into this buffer set by this routine to the start
 *		       of the required information.
 *	      offset - offset into information that we have read upto.
 *	      length - length of buffer
 *	      host_no - host number to return information for
 *	      inout  - 0 for reading, 1 for writing.
 * Returns  : length of data written to buffer.
 */
int eesoxscsi_proc_info(char *buffer, char **start, off_t offset,
			    int length, int host_no, int inout)
{
	struct Scsi_Host *host;
	struct eesoxscsi_info *info;
	char *p = buffer;
	int pos;

	host = scsi_host_hn_get(host_no);
	if (!host)
		return 0;

	if (inout == 1)
		return eesoxscsi_set_proc_info(host, buffer, length);

	info = (struct eesoxscsi_info *)host->hostdata;

	p += sprintf(p, "EESOX SCSI driver v%s\n", VERSION);
	p += fas216_print_host(&info->info, p);
	p += sprintf(p, "Term    : o%s\n",
			info->control & EESOX_TERM_ENABLE ? "n" : "ff");

	pos += fas216_print_stats(&info->info, buffer + pos);
	p += fas216_print_stats(&info->info, p);
	p += fas216_print_devices(&info->info, p);

	if (pos > length)
		pos = length;

	return pos;
}

static ssize_t eesoxscsi_show_term(struct device *dev, char *buf)
{
	struct expansion_card *ec = ECARD_DEV(dev);
	struct Scsi_Host *host = ecard_get_drvdata(ec);
	struct eesoxscsi_info *info = (struct eesoxscsi_info *)host->hostdata;

	return sprintf(buf, "%d\n", info->control & EESOX_TERM_ENABLE ? 1 : 0);
}

static ssize_t eesoxscsi_store_term(struct device *dev, const char *buf, size_t len)
{
	struct expansion_card *ec = ECARD_DEV(dev);
	struct Scsi_Host *host = ecard_get_drvdata(ec);
	struct eesoxscsi_info *info = (struct eesoxscsi_info *)host->hostdata;
	unsigned long flags;

	if (len > 1) {
		spin_lock_irqsave(host->host_lock, flags);
		if (buf[0] != '0') {
			info->control |= EESOX_TERM_ENABLE;
		} else {
			info->control &= ~EESOX_TERM_ENABLE;
		}
		writeb(info->control, info->ctl_port);
		spin_unlock_irqrestore(host->host_lock, flags);
	}

	return len;
}

static DEVICE_ATTR(bus_term, S_IRUGO | S_IWUSR,
		   eesoxscsi_show_term, eesoxscsi_store_term);

static Scsi_Host_Template eesox_template = {
	.module				= THIS_MODULE,
	.proc_info			= eesoxscsi_proc_info,
	.name				= "EESOX SCSI",
	.info				= eesoxscsi_info,
	.command			= fas216_command,
	.queuecommand			= fas216_queue_command,
	.eh_host_reset_handler		= fas216_eh_host_reset,
	.eh_bus_reset_handler		= fas216_eh_bus_reset,
	.eh_device_reset_handler	= fas216_eh_device_reset,
	.eh_abort_handler		= fas216_eh_abort,
	.can_queue			= 1,
	.this_id			= 7,
	.sg_tablesize			= SG_ALL,
	.cmd_per_lun			= 1,
	.use_clustering			= DISABLE_CLUSTERING,
	.proc_name			= "eesox",
};

static int __devinit
eesoxscsi_probe(struct expansion_card *ec, const struct ecard_id *id)
{
	struct Scsi_Host *host;
	struct eesoxscsi_info *info;
	unsigned long resbase, reslen;
	unsigned char *base;
	int ret;

	resbase = ecard_resource_start(ec, ECARD_RES_IOCFAST);
	reslen = ecard_resource_len(ec, ECARD_RES_IOCFAST);

	if (!request_mem_region(resbase, reslen, "eesoxscsi")) {
		ret = -EBUSY;
		goto out;
	}

	base = ioremap(resbase, reslen);
	if (!base) {
		ret = -ENOMEM;
		goto out_region;
	}

	host = scsi_register(&eesox_template,
			     sizeof(struct eesoxscsi_info));
	if (!host) {
		ret = -ENOMEM;
		goto out_unmap;
	}

	host->base	  = (unsigned long)base;
	host->irq	  = ec->irq;
	host->dma_channel = ec->dma;

	ecard_set_drvdata(ec, host);

	info = (struct eesoxscsi_info *)host->hostdata;
	info->ec	= ec;
	info->ctl_port	= base + EESOX_CONTROL;
	info->control	= term[ec->slot_no] ? EESOX_TERM_ENABLE : 0;
	writeb(info->control, info->ctl_port);

	ec->irqaddr	= base + EESOX_DMASTAT;
	ec->irqmask	= EESOX_STAT_INTR;
	ec->irq_data	= info;
	ec->ops		= &eesoxscsi_ops;

	info->info.scsi.io_base		= base + EESOX_FAS216_OFFSET;
	info->info.scsi.io_shift	= EESOX_FAS216_SHIFT;
	info->info.scsi.irq		= host->irq;
	info->info.ifcfg.clockrate	= 40; /* MHz */
	info->info.ifcfg.select_timeout	= 255;
	info->info.ifcfg.asyncperiod	= 200; /* ns */
	info->info.ifcfg.sync_max_depth	= 7;
	info->info.ifcfg.cntl3		= CNTL3_FASTSCSI | CNTL3_FASTCLK;
	info->info.ifcfg.disconnect_ok	= 1;
	info->info.ifcfg.wide_max_size	= 0;
	info->info.ifcfg.capabilities	= FASCAP_PSEUDODMA;
	info->info.dma.setup		= eesoxscsi_dma_setup;
	info->info.dma.pseudo		= eesoxscsi_dma_pseudo;
	info->info.dma.stop		= eesoxscsi_dma_stop;

	device_create_file(&ec->dev, &dev_attr_bus_term);

	ret = fas216_init(host);
	if (ret)
		goto out_free;

	ret = request_irq(host->irq, eesoxscsi_intr, 0, "eesoxscsi", info);
	if (ret) {
		printk("scsi%d: IRQ%d not free: %d\n",
		       host->host_no, host->irq, ret);
		goto out_remove;
	}

	if (host->dma_channel != NO_DMA) {
		if (request_dma(host->dma_channel, "eesox")) {
			printk("scsi%d: DMA%d not free, DMA disabled\n",
			       host->host_no, host->dma_channel);
			host->dma_channel = NO_DMA;
		} else {
			set_dma_speed(host->dma_channel, 180);
			info->info.ifcfg.capabilities |= FASCAP_DMA;
			info->info.ifcfg.cntl3 |= CNTL3_BS8;
		}
	}

	ret = fas216_add(host, &ec->dev);
	if (ret == 0)
		goto out;

	if (host->dma_channel != NO_DMA)
		free_dma(host->dma_channel);
	free_irq(host->irq, host);

 out_remove:
	fas216_remove(host);

 out_free:
	device_remove_file(&ec->dev, &dev_attr_bus_term);
	scsi_unregister(host);

 out_unmap:
	iounmap(base);

 out_region:
	release_mem_region(resbase, reslen);

 out:
	return ret;
}

static void __devexit eesoxscsi_remove(struct expansion_card *ec)
{
	struct Scsi_Host *host = ecard_get_drvdata(ec);
	struct eesoxscsi_info *info = (struct eesoxscsi_info *)host->hostdata;
	unsigned long resbase, reslen;

	ecard_set_drvdata(ec, NULL);
	fas216_remove(host);

	if (host->dma_channel != NO_DMA)
		free_dma(host->dma_channel);
	free_irq(host->irq, info);

	device_remove_file(&ec->dev, &dev_attr_bus_term);

	iounmap((void *)host->base);

	resbase = ecard_resource_start(ec, ECARD_RES_IOCFAST);
	reslen = ecard_resource_len(ec, ECARD_RES_IOCFAST);

	release_mem_region(resbase, reslen);

	fas216_release(host);
	scsi_unregister(host);
}

static const struct ecard_id eesoxscsi_cids[] = {
	{ MANU_EESOX, PROD_EESOX_SCSI2 },
	{ 0xffff, 0xffff },
};

static struct ecard_driver eesoxscsi_driver = {
	.probe		= eesoxscsi_probe,
	.remove		= __devexit_p(eesoxscsi_remove),
	.id_table	= eesoxscsi_cids,
	.drv = {
		.name		= "eesoxscsi",
	},
};

static int __init eesox_init(void)
{
	return ecard_register_driver(&eesoxscsi_driver);
}

static void __exit eesox_exit(void)
{
	ecard_remove_driver(&eesoxscsi_driver);
}

module_init(eesox_init);
module_exit(eesox_exit);

MODULE_AUTHOR("Russell King");
MODULE_DESCRIPTION("EESOX 'Fast' SCSI driver for Acorn machines");
MODULE_PARM(term, "1-8i");
MODULE_PARM_DESC(term, "SCSI bus termination");
MODULE_LICENSE("GPL");

