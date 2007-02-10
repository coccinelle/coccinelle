/*
 * Intel Multimedia Timer device implementation for SGI SN platforms.
 *
 * This file is subject to the terms and conditions of the GNU General Public
 * License.  See the file "COPYING" in the main directory of this archive
 * for more details.
 *
 * Copyright (c) 2001-2004 Silicon Graphics, Inc.  All rights reserved.
 *
 * This driver exports an API that should be supportable by any HPET or IA-PC
 * multimedia timer.  The code below is currently specific to the SGI Altix
 * SHub RTC, however.
 *
 * 11/01/01 - jbarnes - initial revision
 * 9/10/04 - Christoph Lameter - remove interrupt support for kernel inclusion
 */

#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/ioctl.h>
#include <linux/module.h>
#include <linux/init.h>
#include <linux/errno.h>
#include <linux/mm.h>
#include <linux/devfs_fs_kernel.h>
#include <linux/mmtimer.h>
#include <linux/miscdevice.h>
#include <asm/uaccess.h>
#include <asm/sn/addrs.h>
#include <asm/sn/clksupport.h>
#include <asm/sn/shub_mmr.h>

MODULE_AUTHOR("Jesse Barnes <jbarnes@sgi.com>");
MODULE_DESCRIPTION("Multimedia timer support");
MODULE_LICENSE("GPL");

/* name of the device, usually in /dev */
#define MMTIMER_NAME "mmtimer"
#define MMTIMER_DESC "IA-PC Multimedia Timer"
#define MMTIMER_VERSION "1.0"

#define RTC_BITS 55 /* 55 bits for this implementation */

static int mmtimer_ioctl(struct inode *inode, struct file *file,
			 unsigned int cmd, unsigned long arg);
static int mmtimer_mmap(struct file *file, struct vm_area_struct *vma);

/*
 * Period in femtoseconds (10^-15 s)
 */
static unsigned long mmtimer_femtoperiod = 0;

static struct file_operations mmtimer_fops = {
	.owner =	THIS_MODULE,
	.mmap =		mmtimer_mmap,
	.ioctl =	mmtimer_ioctl,
};

/**
 * mmtimer_ioctl - ioctl interface for /dev/mmtimer
 * @inode: inode of the device
 * @file: file structure for the device
 * @cmd: command to execute
 * @arg: optional argument to command
 *
 * Executes the command specified by @cmd.  Returns 0 for success, < 0 for
 * failure.
 *
 * Valid commands:
 *
 * %MMTIMER_GETOFFSET - Should return the offset (relative to the start
 * of the page where the registers are mapped) for the counter in question.
 *
 * %MMTIMER_GETRES - Returns the resolution of the clock in femto (10^-15)
 * seconds
 *
 * %MMTIMER_GETFREQ - Copies the frequency of the clock in Hz to the address
 * specified by @arg
 *
 * %MMTIMER_GETBITS - Returns the number of bits in the clock's counter
 *
 * %MMTIMER_MMAPAVAIL - Returns 1 if the registers can be mmap'd into userspace
 *
 * %MMTIMER_GETCOUNTER - Gets the current value in the counter and places it
 * in the address specified by @arg.
 */
static int mmtimer_ioctl(struct inode *inode, struct file *file,
			 unsigned int cmd, unsigned long arg)
{
	int ret = 0;

	switch (cmd) {
	case MMTIMER_GETOFFSET:	/* offset of the counter */
		/*
		 * SN RTC registers are on their own 64k page
		 */
		if(PAGE_SIZE <= (1 << 16))
			ret = (((long)RTC_COUNTER_ADDR) & (PAGE_SIZE-1)) / 8;
		else
			ret = -ENOSYS;
		break;

	case MMTIMER_GETRES: /* resolution of the clock in 10^-15 s */
		if(copy_to_user((unsigned long *)arg, &mmtimer_femtoperiod,
				sizeof(unsigned long)))
			return -EFAULT;
		break;

	case MMTIMER_GETFREQ: /* frequency in Hz */
		if(copy_to_user((unsigned long *)arg,
				&sn_rtc_cycles_per_second,
				sizeof(unsigned long)))
			return -EFAULT;
		ret = 0;
		break;

	case MMTIMER_GETBITS: /* number of bits in the clock */
		ret = RTC_BITS;
		break;

	case MMTIMER_MMAPAVAIL: /* can we mmap the clock into userspace? */
		ret = (PAGE_SIZE <= (1 << 16)) ? 1 : 0;
		break;

	case MMTIMER_GETCOUNTER:
		if(copy_to_user((unsigned long *)arg, RTC_COUNTER_ADDR,
				sizeof(unsigned long)))
			return -EFAULT;
		break;
	default:
		ret = -ENOSYS;
		break;
	}

	return ret;
}

/**
 * mmtimer_mmap - maps the clock's registers into userspace
 * @file: file structure for the device
 * @vma: VMA to map the registers into
 *
 * Calls remap_pfn_range() to map the clock's registers into
 * the calling process' address space.
 */
static int mmtimer_mmap(struct file *file, struct vm_area_struct *vma)
{
	unsigned long mmtimer_addr;

	if (vma->vm_end - vma->vm_start != PAGE_SIZE)
		return -EINVAL;

	if (vma->vm_flags & VM_WRITE)
		return -EPERM;

	if (PAGE_SIZE > (1 << 16))
		return -ENOSYS;

	vma->vm_flags |= (VM_IO | VM_SHM | VM_LOCKED );
	vma->vm_page_prot = pgprot_noncached(vma->vm_page_prot);

	mmtimer_addr = __pa(RTC_COUNTER_ADDR);
	mmtimer_addr &= ~(PAGE_SIZE - 1);
	mmtimer_addr &= 0xfffffffffffffffUL;

	if (remap_pfn_range(vma, vma->vm_start, mmtimer_addr >> PAGE_SHIFT,
					PAGE_SIZE, vma->vm_page_prot)) {
		printk(KERN_ERR "remap_page_range failed in mmtimer.c\n");
		return -EAGAIN;
	}

	return 0;
}

static struct miscdevice mmtimer_miscdev = {
	SGI_MMTIMER,
	MMTIMER_NAME,
	&mmtimer_fops
};

/**
 * mmtimer_init - device initialization routine
 *
 * Does initial setup for the mmtimer device.
 */
static int __init mmtimer_init(void)
{
	if (!ia64_platform_is("sn2"))
		return -1;

	/*
	 * Sanity check the cycles/sec variable
	 */
	if (sn_rtc_cycles_per_second < 100000) {
		printk(KERN_ERR "%s: unable to determine clock frequency\n",
		       MMTIMER_NAME);
		return -1;
	}

	mmtimer_femtoperiod = ((unsigned long)1E15 + sn_rtc_cycles_per_second /
			       2) / sn_rtc_cycles_per_second;

	strcpy(mmtimer_miscdev.devfs_name, MMTIMER_NAME);
	if (misc_register(&mmtimer_miscdev)) {
		printk(KERN_ERR "%s: failed to register device\n",
		       MMTIMER_NAME);
		return -1;
	}

	printk(KERN_INFO "%s: v%s, %ld MHz\n", MMTIMER_DESC, MMTIMER_VERSION,
	       sn_rtc_cycles_per_second/(unsigned long)1E6);

	return 0;
}

module_init(mmtimer_init);

