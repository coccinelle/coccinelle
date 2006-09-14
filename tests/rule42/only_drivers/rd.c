/*
 * ramdisk.c - Multiple RAM disk driver - gzip-loading version - v. 0.8 beta.
 * 
 * (C) Chad Page, Theodore Ts'o, et. al, 1995. 
 *
 * This RAM disk is designed to have filesystems created on it and mounted
 * just like a regular floppy disk.  
 *  
 * It also does something suggested by Linus: use the buffer cache as the
 * RAM disk data.  This makes it possible to dynamically allocate the RAM disk
 * buffer - with some consequences I have to deal with as I write this. 
 * 
 * This code is based on the original ramdisk.c, written mostly by
 * Theodore Ts'o (TYT) in 1991.  The code was largely rewritten by
 * Chad Page to use the buffer cache to store the RAM disk data in
 * 1995; Theodore then took over the driver again, and cleaned it up
 * for inclusion in the mainline kernel.
 *
 * The original CRAMDISK code was written by Richard Lyons, and
 * adapted by Chad Page to use the new RAM disk interface.  Theodore
 * Ts'o rewrote it so that both the compressed RAM disk loader and the
 * kernel decompressor uses the same inflate.c codebase.  The RAM disk
 * loader now also loads into a dynamic (buffer cache based) RAM disk,
 * not the old static RAM disk.  Support for the old static RAM disk has
 * been completely removed.
 *
 * Loadable module support added by Tom Dyas.
 *
 * Further cleanups by Chad Page (page0588@sundance.sjsu.edu):
 *	Cosmetic changes in #ifdef MODULE, code movement, etc.
 * 	When the RAM disk module is removed, free the protected buffers
 * 	Default RAM disk size changed to 2.88 MB
 *
 *  Added initrd: Werner Almesberger & Hans Lermen, Feb '96
 *
 * 4/25/96 : Made RAM disk size a parameter (default is now 4 MB) 
 *		- Chad Page
 *
 * Add support for fs images split across >1 disk, Paul Gortmaker, Mar '98
 *
 * Make block size and block size shift for RAM disks a global macro
 * and set blk_size for -ENOSPC,     Werner Fink <werner@suse.de>, Apr '99
 */

#include <linux/config.h>
#include <linux/string.h>
#include <linux/slab.h>
#include <linux/module.h>
#include <linux/init.h>
#include <linux/devfs_fs_kernel.h>
#include <asm/uaccess.h>

/*
 * 35 has been officially registered as the RAMDISK major number, but
 * so is the original MAJOR number of 1.  We're using 1 in
 * include/linux/major.h for now
 */
#define MAJOR_NR RAMDISK_MAJOR
#include <linux/blk.h>
#include <linux/blkpg.h>

/* The RAM disk size is now a parameter */
#define NUM_RAMDISKS 16		/* This cannot be overridden (yet) */ 

#ifdef CONFIG_BLK_DEV_INITRD
static int initrd_users;
static spinlock_t initrd_users_lock = SPIN_LOCK_UNLOCKED;
unsigned long initrd_start, initrd_end;
int initrd_below_start_ok;
#endif

/* Various static variables go here.  Most are used only in the RAM disk code.
 */

static unsigned long rd_length[NUM_RAMDISKS];	/* Size of RAM disks in bytes   */
static int rd_hardsec[NUM_RAMDISKS];		/* Size of real blocks in bytes */
static int rd_blocksizes[NUM_RAMDISKS];		/* Size of 1024 byte blocks :)  */
static int rd_kbsize[NUM_RAMDISKS];	/* Size in blocks of 1024 bytes */
static devfs_handle_t devfs_handle;
static struct block_device *rd_bdev[NUM_RAMDISKS];/* Protected device data */

/*
 * Parameters for the boot-loading of the RAM disk.  These are set by
 * init/main.c (from arguments to the kernel command line) or from the
 * architecture-specific setup routine (from the stored boot sector
 * information). 
 */
int rd_size = CONFIG_BLK_DEV_RAM_SIZE;		/* Size of the RAM disks */
/*
 * It would be very desiderable to have a soft-blocksize (that in the case
 * of the ramdisk driver is also the hardblocksize ;) of PAGE_SIZE because
 * doing that we'll achieve a far better MM footprint. Using a rd_blocksize of
 * BLOCK_SIZE in the worst case we'll make PAGE_SIZE/BLOCK_SIZE buffer-pages
 * unfreeable. With a rd_blocksize of PAGE_SIZE instead we are sure that only
 * 1 page will be protected. Depending on the size of the ramdisk you
 * may want to change the ramdisk blocksize to achieve a better or worse MM
 * behaviour. The default is still BLOCK_SIZE (needed by rd_load_image that
 * supposes the filesystem in the image uses a BLOCK_SIZE blocksize).
 */
int rd_blocksize = BLOCK_SIZE;			/* blocksize of the RAM disks */

/*
 * Copyright (C) 2000 Linus Torvalds.
 *               2000 Transmeta Corp.
 * aops copied from ramfs.
 */
static int ramdisk_readpage(struct file *file, struct page * page)
{
	if (!Page_Uptodate(page)) {
		memset(kmap(page), 0, PAGE_CACHE_SIZE);
		kunmap(page);
		flush_dcache_page(page);
		SetPageUptodate(page);
	}
	UnlockPage(page);
	return 0;
}

static int ramdisk_prepare_write(struct file *file, struct page *page, unsigned offset, unsigned to)
{
	if (!Page_Uptodate(page)) {
		void *addr = page_address(page);
		memset(addr, 0, PAGE_CACHE_SIZE);
		flush_dcache_page(page);
		SetPageUptodate(page);
	}
	SetPageDirty(page);
	return 0;
}

static int ramdisk_commit_write(struct file *file, struct page *page, unsigned offset, unsigned to)
{
	return 0;
}

static struct address_space_operations ramdisk_aops = {
	readpage: ramdisk_readpage,
	writepage: fail_writepage,
	prepare_write: ramdisk_prepare_write,
	commit_write: ramdisk_commit_write,
};

static int rd_blkdev_pagecache_IO(int rw, struct bio_vec *vec,
				  sector_t sector, int minor)
{
	struct address_space * mapping;
	unsigned long index;
	int offset, size, err;

	err = 0;
	mapping = rd_bdev[minor]->bd_inode->i_mapping;

	index = sector >> (PAGE_CACHE_SHIFT - 9);
	offset = (sector << 9) & ~PAGE_CACHE_MASK;
	size = vec->bv_len;

	do {
		int count;
		struct page * page;
		char * src, * dst;
		int unlock = 0;

		count = PAGE_CACHE_SIZE - offset;
		if (count > size)
			count = size;
		size -= count;

		page = find_get_page(mapping, index);
		if (!page) {
			page = grab_cache_page(mapping, index);
			err = -ENOMEM;
			if (!page)
				goto out;
			err = 0;

			if (!Page_Uptodate(page)) {
				memset(kmap(page), 0, PAGE_CACHE_SIZE);
				kunmap(page);
				SetPageUptodate(page);
			}

			unlock = 1;
		}

		index++;

		if (rw == READ) {
			src = kmap(page);
			src += offset;
			dst = kmap(vec->bv_page) + vec->bv_offset;
		} else {
			dst = kmap(page);
			dst += offset;
			src = kmap(vec->bv_page) + vec->bv_offset;
		}
		offset = 0;

		memcpy(dst, src, count);

		kunmap(page);
		kunmap(vec->bv_page);

		if (rw == READ) {
			flush_dcache_page(page);
		} else {
			SetPageDirty(page);
		}
		if (unlock)
			UnlockPage(page);
		__free_page(page);
	} while (size);

 out:
	return err;
}

static int rd_blkdev_bio_IO(struct bio *bio, unsigned int minor)
{
	struct bio_vec *bvec;
	sector_t sector;
	int ret = 0, i, rw;

	sector = bio->bi_sector;
	rw = bio_data_dir(bio);
	bio_for_each_segment(bvec, bio, i) {
		ret |= rd_blkdev_pagecache_IO(rw, bvec, sector, minor);
		sector += bvec->bv_len >> 9;
	}

	return ret;
}

/*
 *  Basically, my strategy here is to set up a buffer-head which can't be
 *  deleted, and make that my Ramdisk.  If the request is outside of the
 *  allocated size, we must get rid of it...
 *
 * 19-JAN-1998  Richard Gooch <rgooch@atnf.csiro.au>  Added devfs support
 *
 */
static int rd_make_request(request_queue_t * q, struct bio *sbh)
{
	unsigned int minor;
	unsigned long offset, len;
	int rw = sbh->bi_rw;

	minor = minor(to_kdev_t(sbh->bi_bdev->bd_dev));

	if (minor >= NUM_RAMDISKS)
		goto fail;

	offset = sbh->bi_sector << 9;
	len = sbh->bi_size;

	if ((offset + len) > rd_length[minor])
		goto fail;

	if (rw==READA)
		rw=READ;
	if ((rw != READ) && (rw != WRITE)) {
		printk(KERN_INFO "RAMDISK: bad command: %d\n", rw);
		goto fail;
	}

	if (rd_blkdev_bio_IO(sbh, minor))
		goto fail;

	set_bit(BIO_UPTODATE, &sbh->bi_flags);
	sbh->bi_end_io(sbh);
	return 0;
 fail:
	bio_io_error(sbh);
	return 0;
} 

static int rd_ioctl(struct inode *inode, struct file *file, unsigned int cmd, unsigned long arg)
{
	int error = -EINVAL;
	unsigned int minor;

	if (!inode || kdev_none(inode->i_rdev))
		goto out;

	minor = minor(inode->i_rdev);

	switch (cmd) {
		case BLKFLSBUF:
			if (!capable(CAP_SYS_ADMIN))
				return -EACCES;
			/* special: we want to release the ramdisk memory,
			   it's not like with the other blockdevices where
			   this ioctl only flushes away the buffer cache. */
			error = -EBUSY;
			down(&inode->i_bdev->bd_sem);
			if (inode->i_bdev->bd_openers <= 2) {
				truncate_inode_pages(inode->i_mapping, 0);
				error = 0;
			}
			up(&inode->i_bdev->bd_sem);
			break;
         	case BLKGETSIZE:
         	case BLKGETSIZE64:
		case BLKROSET:
		case BLKROGET:
		case BLKSSZGET:
			error = blk_ioctl(inode->i_bdev, cmd, arg);
	}
out:
	return error;
}


#ifdef CONFIG_BLK_DEV_INITRD

static ssize_t initrd_read(struct file *file, char *buf,
			   size_t count, loff_t *ppos)
{
	int left;

	left = initrd_end - initrd_start - *ppos;
	if (count > left) count = left;
	if (count == 0) return 0;
	copy_to_user(buf, (char *)initrd_start + *ppos, count);
	*ppos += count;
	return count;
}


static int initrd_release(struct inode *inode,struct file *file)
{
	extern void free_initrd_mem(unsigned long, unsigned long);

	spin_lock(&initrd_users_lock);
	if (!--initrd_users) {
		spin_unlock(&initrd_users_lock);
		free_initrd_mem(initrd_start, initrd_end);
		initrd_start = 0;
	} else {
		spin_unlock(&initrd_users_lock);
	}
		
	blkdev_put(inode->i_bdev, BDEV_FILE);
	return 0;
}


static struct file_operations initrd_fops = {
	read:		initrd_read,
	release:	initrd_release,
};

#endif


static int rd_open(struct inode * inode, struct file * filp)
{
	int unit = DEVICE_NR(inode->i_rdev);

#ifdef CONFIG_BLK_DEV_INITRD
	if (unit == INITRD_MINOR) {
		spin_lock(&initrd_users_lock);
		initrd_users++;
		spin_unlock(&initrd_users_lock);
		if (!initrd_start) 
			return -ENODEV;
		filp->f_op = &initrd_fops;
		return 0;
	}
#endif

	if (unit >= NUM_RAMDISKS)
		return -ENXIO;

	/*
	 * Immunize device against invalidate_buffers() and prune_icache().
	 */
	if (rd_bdev[unit] == NULL) {
		rd_bdev[unit] = bdget(kdev_t_to_nr(inode->i_rdev));
		rd_bdev[unit]->bd_openers++;
		rd_bdev[unit]->bd_inode->i_mapping->a_ops = &ramdisk_aops;
	}

	return 0;
}

static struct block_device_operations rd_bd_op = {
	owner:		THIS_MODULE,
	open:		rd_open,
	ioctl:		rd_ioctl,
};

/* Before freeing the module, invalidate all of the protected buffers! */
static void __exit rd_cleanup (void)
{
	int i;

	for (i = 0 ; i < NUM_RAMDISKS; i++) {
		struct block_device *bdev = rd_bdev[i];
		rd_bdev[i] = NULL;
		if (bdev) {
			invalidate_bdev(bdev, 1);
			blkdev_put(bdev, BDEV_FILE);
		}
	}

	devfs_unregister (devfs_handle);
	unregister_blkdev( MAJOR_NR, "ramdisk" );
	blk_clear(MAJOR_NR);
}

/* This is the registration and initialization section of the RAM disk driver */
static int __init rd_init (void)
{
	int i;

	if (rd_blocksize > PAGE_SIZE || rd_blocksize < 512 ||
	    (rd_blocksize & (rd_blocksize-1))) {
		printk("RAMDISK: wrong blocksize %d, reverting to defaults\n",
		       rd_blocksize);
		rd_blocksize = BLOCK_SIZE;
	}

	if (register_blkdev(MAJOR_NR, "ramdisk", &rd_bd_op)) {
		printk("RAMDISK: Could not get major %d", MAJOR_NR);
		return -EIO;
	}

	blk_queue_make_request(BLK_DEFAULT_QUEUE(MAJOR_NR), &rd_make_request);

	for (i = 0; i < NUM_RAMDISKS; i++) {
		/* rd_size is given in kB */
		rd_length[i] = rd_size << 10;
		rd_hardsec[i] = rd_blocksize;
		rd_blocksizes[i] = rd_blocksize;
		rd_kbsize[i] = rd_size;
	}
	devfs_handle = devfs_mk_dir (NULL, "rd", NULL);
	devfs_register_series (devfs_handle, "%u", NUM_RAMDISKS,
			       DEVFS_FL_DEFAULT, MAJOR_NR, 0,
			       S_IFBLK | S_IRUSR | S_IWUSR,
			       &rd_bd_op, NULL);

	for (i = 0; i < NUM_RAMDISKS; i++)
		register_disk(NULL, mk_kdev(MAJOR_NR,i), 1, &rd_bd_op,
			      rd_size<<1);

#ifdef CONFIG_BLK_DEV_INITRD
	/* We ought to separate initrd operations here */
	register_disk(NULL, mk_kdev(MAJOR_NR,INITRD_MINOR), 1, &rd_bd_op, rd_size<<1);
	devfs_register(devfs_handle, "initrd", DEVFS_FL_DEFAULT, MAJOR_NR,
			INITRD_MINOR, S_IFBLK | S_IRUSR, &rd_bd_op, NULL);
#endif

	blksize_size[MAJOR_NR] = rd_blocksizes;	/* Avoid set_blocksize() check */
	blk_size[MAJOR_NR] = rd_kbsize;		/* Size of the RAM disk in kB  */

	/* rd_size is given in kB */
	printk("RAMDISK driver initialized: "
	       "%d RAM disks of %dK size %d blocksize\n",
	       NUM_RAMDISKS, rd_size, rd_blocksize);

	return 0;
}

module_init(rd_init);
module_exit(rd_cleanup);

/* options - nonmodular */
#ifndef MODULE
static int __init ramdisk_size(char *str)
{
	rd_size = simple_strtol(str,NULL,0);
	return 1;
}
static int __init ramdisk_size2(char *str)	/* kludge */
{
	return ramdisk_size(str);
}
static int __init ramdisk_blocksize(char *str)
{
	rd_blocksize = simple_strtol(str,NULL,0);
	return 1;
}
__setup("ramdisk=", ramdisk_size);
__setup("ramdisk_size=", ramdisk_size2);
__setup("ramdisk_blocksize=", ramdisk_blocksize);
#endif

/* options - modular */
MODULE_PARM     (rd_size, "1i");
MODULE_PARM_DESC(rd_size, "Size of each RAM disk in kbytes.");
MODULE_PARM     (rd_blocksize, "i");
MODULE_PARM_DESC(rd_blocksize, "Blocksize of each RAM disk in bytes.");

MODULE_LICENSE("GPL");
