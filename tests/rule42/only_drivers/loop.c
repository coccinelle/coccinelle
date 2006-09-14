/*
 *  linux/drivers/block/loop.c
 *
 *  Written by Theodore Ts'o, 3/29/93
 * 
 * Copyright 1993 by Theodore Ts'o.  Redistribution of this file is
 * permitted under the GNU General Public License.
 *
 * DES encryption plus some minor changes by Werner Almesberger, 30-MAY-1993
 * more DES encryption plus IDEA encryption by Nicholas J. Leon, June 20, 1996
 *
 * Modularized and updated for 1.1.16 kernel - Mitch Dsouza 28th May 1994
 * Adapted for 1.3.59 kernel - Andries Brouwer, 1 Feb 1996
 *
 * Fixed do_loop_request() re-entrancy - Vincent.Renardias@waw.com Mar 20, 1997
 *
 * Added devfs support - Richard Gooch <rgooch@atnf.csiro.au> 16-Jan-1998
 *
 * Handle sparse backing files correctly - Kenn Humborg, Jun 28, 1998
 *
 * Loadable modules and other fixes by AK, 1998
 *
 * Make real block number available to downstream transfer functions, enables
 * CBC (and relatives) mode encryption requiring unique IVs per data block. 
 * Reed H. Petty, rhp@draper.net
 *
 * Maximum number of loop devices now dynamic via max_loop module parameter.
 * Russell Kroll <rkroll@exploits.org> 19990701
 * 
 * Maximum number of loop devices when compiled-in now selectable by passing
 * max_loop=<1-255> to the kernel on boot.
 * Erik I. Bolsø, <eriki@himolde.no>, Oct 31, 1999
 *
 * Completely rewrite request handling to be make_request_fn style and
 * non blocking, pushing work to a helper thread. Lots of fixes from
 * Al Viro too.
 * Jens Axboe <axboe@suse.de>, Nov 2000
 *
 * Support up to 256 loop devices
 * Heinz Mauelshagen <mge@sistina.com>, Feb 2002
 *
 * Still To Fix:
 * - Advisory locking is ignored here. 
 * - Should use an own CAP_* category instead of CAP_SYS_ADMIN 
 *
 * WARNING/FIXME:
 * - The block number as IV passing to low level transfer functions is broken:
 *   it passes the underlying device's block number instead of the
 *   offset. This makes it change for a given block when the file is 
 *   moved/restored/copied and also doesn't work over NFS. 
 * AV, Feb 12, 2000: we pass the logical block number now. It fixes the
 *   problem above. Encryption modules that used to rely on the old scheme
 *   should just call ->i_mapping->bmap() to calculate the physical block
 *   number.
 */ 

#include <linux/config.h>
#include <linux/module.h>

#include <linux/sched.h>
#include <linux/fs.h>
#include <linux/file.h>
#include <linux/stat.h>
#include <linux/errno.h>
#include <linux/major.h>
#include <linux/wait.h>
#include <linux/blk.h>
#include <linux/blkpg.h>
#include <linux/init.h>
#include <linux/devfs_fs_kernel.h>
#include <linux/smp_lock.h>
#include <linux/swap.h>
#include <linux/slab.h>

#include <asm/uaccess.h>

#include <linux/loop.h>		

#define MAJOR_NR LOOP_MAJOR

static int max_loop = 8;
static struct loop_device *loop_dev;
static int *loop_sizes;
static int *loop_blksizes;
static devfs_handle_t devfs_handle;      /*  For the directory */

/*
 * Transfer functions
 */
static int transfer_none(struct loop_device *lo, int cmd, char *raw_buf,
			 char *loop_buf, int size, int real_block)
{
	if (raw_buf != loop_buf) {
		if (cmd == READ)
			memcpy(loop_buf, raw_buf, size);
		else
			memcpy(raw_buf, loop_buf, size);
	}

	return 0;
}

static int transfer_xor(struct loop_device *lo, int cmd, char *raw_buf,
			char *loop_buf, int size, int real_block)
{
	char	*in, *out, *key;
	int	i, keysize;

	if (cmd == READ) {
		in = raw_buf;
		out = loop_buf;
	} else {
		in = loop_buf;
		out = raw_buf;
	}

	key = lo->lo_encrypt_key;
	keysize = lo->lo_encrypt_key_size;
	for (i = 0; i < size; i++)
		*out++ = *in++ ^ key[(i & 511) % keysize];
	return 0;
}

static int none_status(struct loop_device *lo, struct loop_info *info)
{
	lo->lo_flags |= LO_FLAGS_BH_REMAP;
	return 0;
}

static int xor_status(struct loop_device *lo, struct loop_info *info)
{
	if (info->lo_encrypt_key_size <= 0)
		return -EINVAL;
	return 0;
}

struct loop_func_table none_funcs = { 
	number: LO_CRYPT_NONE,
	transfer: transfer_none,
	init: none_status,
}; 	

struct loop_func_table xor_funcs = { 
	number: LO_CRYPT_XOR,
	transfer: transfer_xor,
	init: xor_status
}; 	

/* xfer_funcs[0] is special - its release function is never called */ 
struct loop_func_table *xfer_funcs[MAX_LO_CRYPT] = {
	&none_funcs,
	&xor_funcs  
};

#define MAX_DISK_SIZE 1024*1024*1024

static unsigned long
compute_loop_size(struct loop_device *lo, struct dentry * lo_dentry)
{
	loff_t size = lo_dentry->d_inode->i_mapping->host->i_size;
	return (size - lo->lo_offset) >> BLOCK_SIZE_BITS;
}

static void figure_loop_size(struct loop_device *lo)
{
	loop_sizes[lo->lo_number] = compute_loop_size(lo,
					lo->lo_backing_file->f_dentry);
					
}

static int lo_send(struct loop_device *lo, struct bio *bio, int bsize, loff_t pos)
{
	struct file *file = lo->lo_backing_file; /* kudos to NFsckingS */
	struct address_space *mapping = file->f_dentry->d_inode->i_mapping;
	struct address_space_operations *aops = mapping->a_ops;
	struct page *page;
	char *kaddr, *data;
	unsigned long index;
	unsigned size, offset;
	int len;

	down(&mapping->host->i_sem);
	index = pos >> PAGE_CACHE_SHIFT;
	offset = pos & (PAGE_CACHE_SIZE - 1);
	len = bio->bi_size;
	data = bio_data(bio);
	while (len > 0) {
		int IV = index * (PAGE_CACHE_SIZE/bsize) + offset/bsize;
		int transfer_result;

		size = PAGE_CACHE_SIZE - offset;
		if (size > len)
			size = len;

		page = grab_cache_page(mapping, index);
		if (!page)
			goto fail;
		if (aops->prepare_write(file, page, offset, offset+size))
			goto unlock;
		kaddr = page_address(page);
		flush_dcache_page(page);
		transfer_result = lo_do_transfer(lo, WRITE, kaddr + offset, data, size, IV);
		if (transfer_result) {
			/*
			 * The transfer failed, but we still write the data to
			 * keep prepare/commit calls balanced.
			 */
			printk(KERN_ERR "loop: transfer error block %ld\n", index);
			memset(kaddr + offset, 0, size);
		}
		if (aops->commit_write(file, page, offset, offset+size))
			goto unlock;
		if (transfer_result)
			goto unlock;
		data += size;
		len -= size;
		offset = 0;
		index++;
		pos += size;
		UnlockPage(page);
		page_cache_release(page);
	}
	up(&mapping->host->i_sem);
	return 0;

unlock:
	UnlockPage(page);
	page_cache_release(page);
fail:
	up(&mapping->host->i_sem);
	return -1;
}

struct lo_read_data {
	struct loop_device *lo;
	char *data;
	int bsize;
};

static int lo_read_actor(read_descriptor_t * desc, struct page *page, unsigned long offset, unsigned long size)
{
	char *kaddr;
	unsigned long count = desc->count;
	struct lo_read_data *p = (struct lo_read_data*)desc->buf;
	struct loop_device *lo = p->lo;
	int IV = page->index * (PAGE_CACHE_SIZE/p->bsize) + offset/p->bsize;

	if (size > count)
		size = count;

	kaddr = kmap(page);
	if (lo_do_transfer(lo, READ, kaddr + offset, p->data, size, IV)) {
		size = 0;
		printk(KERN_ERR "loop: transfer error block %ld\n",page->index);
		desc->error = -EINVAL;
	}
	kunmap(page);
	
	desc->count = count - size;
	desc->written += size;
	p->data += size;
	return size;
}

static int lo_receive(struct loop_device *lo, struct bio *bio, int bsize, loff_t pos)
{
	struct lo_read_data cookie;
	read_descriptor_t desc;
	struct file *file;

	cookie.lo = lo;
	cookie.data = bio_data(bio);
	cookie.bsize = bsize;
	desc.written = 0;
	desc.count = bio->bi_size;
	desc.buf = (char*)&cookie;
	desc.error = 0;
	spin_lock_irq(&lo->lo_lock);
	file = lo->lo_backing_file;
	spin_unlock_irq(&lo->lo_lock);
	do_generic_file_read(file, &pos, &desc, lo_read_actor);
	return desc.error;
}

static inline int loop_get_bs(struct loop_device *lo)
{
	return block_size(to_kdev_t(lo->lo_device->bd_dev));
}

static inline unsigned long loop_get_iv(struct loop_device *lo,
					unsigned long sector)
{
	int bs = loop_get_bs(lo);
	unsigned long offset, IV;

	IV = sector / (bs >> 9) + lo->lo_offset / bs;
	offset = ((sector % (bs >> 9)) << 9) + lo->lo_offset % bs;
	if (offset >= bs)
		IV++;

	return IV;
}

static int do_bio_filebacked(struct loop_device *lo, struct bio *bio)
{
	loff_t pos;
	int ret;

	pos = ((loff_t) bio->bi_sector << 9) + lo->lo_offset;

	do {
		if (bio_rw(bio) == WRITE)
			ret = lo_send(lo, bio, loop_get_bs(lo), pos);
		else
			ret = lo_receive(lo, bio, loop_get_bs(lo), pos);

	} while (++bio->bi_idx < bio->bi_vcnt);

	return ret;
}

static void loop_end_io_transfer(struct bio *);
static void loop_put_buffer(struct bio *bio)
{
	/*
	 * check bi_end_io, may just be a remapped bio
	 */
	if (bio && bio->bi_end_io == loop_end_io_transfer) {
		int i;
		for (i = 0; i < bio->bi_vcnt; i++)
			__free_page(bio->bi_io_vec[i].bv_page);

		bio_put(bio);
	}
}

/*
 * Add bio to back of pending list
 */
static void loop_add_bio(struct loop_device *lo, struct bio *bio)
{
	unsigned long flags;

	spin_lock_irqsave(&lo->lo_lock, flags);
	if (lo->lo_biotail) {
		lo->lo_biotail->bi_next = bio;
		lo->lo_biotail = bio;
	} else
		lo->lo_bio = lo->lo_biotail = bio;
	spin_unlock_irqrestore(&lo->lo_lock, flags);

	up(&lo->lo_bh_mutex);
}

/*
 * Grab first pending buffer
 */
static struct bio *loop_get_bio(struct loop_device *lo)
{
	struct bio *bio;

	spin_lock_irq(&lo->lo_lock);
	if ((bio = lo->lo_bio)) {
		if (bio == lo->lo_biotail)
			lo->lo_biotail = NULL;
		lo->lo_bio = bio->bi_next;
		bio->bi_next = NULL;
	}
	spin_unlock_irq(&lo->lo_lock);

	return bio;
}

/*
 * if this was a WRITE lo->transfer stuff has already been done. for READs,
 * queue it for the loop thread and let it do the transfer out of
 * bi_end_io context (we don't want to do decrypt of a page with irqs
 * disabled)
 */
static void loop_end_io_transfer(struct bio *bio)
{
	struct bio *rbh = bio->bi_private;
	struct loop_device *lo = &loop_dev[minor(to_kdev_t(rbh->bi_bdev->bd_dev))];
	int uptodate = test_bit(BIO_UPTODATE, &bio->bi_flags);

	if (!uptodate || bio_rw(bio) == WRITE) {
		bio_endio(rbh, uptodate);
		if (atomic_dec_and_test(&lo->lo_pending))
			up(&lo->lo_bh_mutex);
		loop_put_buffer(bio);
	} else
		loop_add_bio(lo, bio);
}

static struct bio *loop_get_buffer(struct loop_device *lo, struct bio *rbh)
{
	struct bio *bio;

	/*
	 * for xfer_funcs that can operate on the same bh, do that
	 */
	if (lo->lo_flags & LO_FLAGS_BH_REMAP) {
		bio = rbh;
		goto out_bh;
	}

	bio = bio_copy(rbh, GFP_NOIO, rbh->bi_rw & WRITE);

	bio->bi_end_io = loop_end_io_transfer;
	bio->bi_private = rbh;

out_bh:
	bio->bi_sector = rbh->bi_sector + (lo->lo_offset >> 9);
	bio->bi_rw = rbh->bi_rw;
	spin_lock_irq(&lo->lo_lock);
	bio->bi_bdev = lo->lo_device;
	spin_unlock_irq(&lo->lo_lock);

	return bio;
}

static int loop_make_request(request_queue_t *q, struct bio *rbh)
{
	struct bio *bh = NULL;
	struct loop_device *lo;
	unsigned long IV;
	int rw = bio_rw(rbh);
	int unit = minor(to_kdev_t(rbh->bi_bdev->bd_dev));

	if (unit >= max_loop)
		goto out;

	lo = &loop_dev[unit];
	spin_lock_irq(&lo->lo_lock);
	if (lo->lo_state != Lo_bound)
		goto inactive;
	atomic_inc(&lo->lo_pending);
	spin_unlock_irq(&lo->lo_lock);

	if (rw == WRITE) {
		if (lo->lo_flags & LO_FLAGS_READ_ONLY)
			goto err;
	} else if (rw == READA) {
		rw = READ;
	} else if (rw != READ) {
		printk(KERN_ERR "loop: unknown command (%x)\n", rw);
		goto err;
	}

	blk_queue_bounce(q, &rbh);

	/*
	 * file backed, queue for loop_thread to handle
	 */
	if (lo->lo_flags & LO_FLAGS_DO_BMAP) {
		loop_add_bio(lo, rbh);
		return 0;
	}

	/*
	 * piggy old buffer on original, and submit for I/O
	 */
	bh = loop_get_buffer(lo, rbh);
	IV = loop_get_iv(lo, rbh->bi_sector);
	if (rw == WRITE) {
		if (lo_do_transfer(lo, WRITE, bio_data(bh), bio_data(rbh),
				   bh->bi_size, IV))
			goto err;
	}

	generic_make_request(bh);
	return 0;

err:
	if (atomic_dec_and_test(&lo->lo_pending))
		up(&lo->lo_bh_mutex);
	loop_put_buffer(bh);
out:
	bio_io_error(rbh);
	return 0;
inactive:
	spin_unlock_irq(&lo->lo_lock);
	goto out;
}

static int do_bio_blockbacked(struct loop_device *lo, struct bio *bio,
			      struct bio *rbh)
{
	unsigned long IV = loop_get_iv(lo, rbh->bi_sector);
	struct bio_vec *from;
	char *vto, *vfrom;
	int ret = 0, i;

	bio_for_each_segment(from, rbh, i) {
		vfrom = page_address(from->bv_page) + from->bv_offset;
		vto = page_address(bio->bi_io_vec[i].bv_page) + bio->bi_io_vec[i].bv_offset;
		ret |= lo_do_transfer(lo, bio_data_dir(bio), vto, vfrom,
					from->bv_len, IV);
	}

	return ret;
}

static inline void loop_handle_bio(struct loop_device *lo, struct bio *bio)
{
	int ret;

	/*
	 * For block backed loop, we know this is a READ
	 */
	if (lo->lo_flags & LO_FLAGS_DO_BMAP) {
		ret = do_bio_filebacked(lo, bio);
		bio_endio(bio, !ret);
	} else {
		struct bio *rbh = bio->bi_private;

		ret = do_bio_blockbacked(lo, bio, rbh);

		bio_endio(rbh, !ret);
		loop_put_buffer(bio);
	}
}

/*
 * worker thread that handles reads/writes to file backed loop devices,
 * to avoid blocking in our make_request_fn. it also does loop decrypting
 * on reads for block backed loop, as that is too heavy to do from
 * b_end_io context where irqs may be disabled.
 */
static int loop_thread(void *data)
{
	struct loop_device *lo = data;
	struct bio *bio;

	daemonize();

	sprintf(current->comm, "loop%d", lo->lo_number);

	spin_lock_irq(&current->sigmask_lock);
	sigfillset(&current->blocked);
	flush_signals(current);
	spin_unlock_irq(&current->sigmask_lock);

	set_user_nice(current, -20);

	spin_lock_irq(&lo->lo_lock);
	lo->lo_state = Lo_bound;
	atomic_inc(&lo->lo_pending);
	spin_unlock_irq(&lo->lo_lock);

	/*
	 * up sem, we are running
	 */
	up(&lo->lo_sem);

	for (;;) {
		down_interruptible(&lo->lo_bh_mutex);
		/*
		 * could be upped because of tear-down, not because of
		 * pending work
		 */
		if (!atomic_read(&lo->lo_pending))
			break;

		bio = loop_get_bio(lo);
		if (!bio) {
			printk("loop: missing bio\n");
			continue;
		}
		loop_handle_bio(lo, bio);

		/*
		 * upped both for pending work and tear-down, lo_pending
		 * will hit zero then
		 */
		if (atomic_dec_and_test(&lo->lo_pending))
			break;
	}

	up(&lo->lo_sem);
	return 0;
}

static int loop_set_fd(struct loop_device *lo, struct file *lo_file,
		       struct block_device *bdev, unsigned int arg)
{
	struct file	*file;
	struct inode	*inode;
	kdev_t		dev = to_kdev_t(bdev->bd_dev);
	struct block_device *lo_device;
	int		lo_flags = 0;
	int		error;

	MOD_INC_USE_COUNT;

	error = -EBUSY;
	if (lo->lo_state != Lo_unbound)
		goto out;

	error = -EBADF;
	file = fget(arg);
	if (!file)
		goto out;

	error = -EINVAL;
	inode = file->f_dentry->d_inode;

	if (!(file->f_mode & FMODE_WRITE))
		lo_flags |= LO_FLAGS_READ_ONLY;

	if (S_ISBLK(inode->i_mode)) {
		lo_device = inode->i_bdev;
		if (lo_device == bdev) {
			error = -EBUSY;
			goto out;
		}
	} else if (S_ISREG(inode->i_mode)) {
		struct address_space_operations *aops = inode->i_mapping->a_ops;
		/*
		 * If we can't read - sorry. If we only can't write - well,
		 * it's going to be read-only.
		 */
		if (!aops->readpage)
			goto out_putf;

		if (!aops->prepare_write || !aops->commit_write)
			lo_flags |= LO_FLAGS_READ_ONLY;

		lo_device = inode->i_sb->s_bdev;
		lo_flags |= LO_FLAGS_DO_BMAP;
		error = 0;
	} else
		goto out_putf;

	get_file(file);

	if (IS_RDONLY (inode) || is_read_only(to_kdev_t(lo_device->bd_dev))
	    || !(lo_file->f_mode & FMODE_WRITE))
		lo_flags |= LO_FLAGS_READ_ONLY;

	set_device_ro(dev, (lo_flags & LO_FLAGS_READ_ONLY) != 0);

	lo->lo_device = lo_device;
	lo->lo_flags = lo_flags;
	lo->lo_backing_file = file;
	lo->transfer = NULL;
	lo->ioctl = NULL;
	figure_loop_size(lo);
	lo->old_gfp_mask = inode->i_mapping->gfp_mask;
	inode->i_mapping->gfp_mask = GFP_NOIO;

	set_blocksize(dev, block_size(to_kdev_t(lo_device->bd_dev)));

	lo->lo_bio = lo->lo_biotail = NULL;
	kernel_thread(loop_thread, lo, CLONE_FS | CLONE_FILES | CLONE_SIGHAND);
	down(&lo->lo_sem);

	fput(file);
	return 0;

 out_putf:
	fput(file);
 out:
	MOD_DEC_USE_COUNT;
	return error;
}

static int loop_release_xfer(struct loop_device *lo)
{
	int err = 0; 
	if (lo->lo_encrypt_type) {
		struct loop_func_table *xfer= xfer_funcs[lo->lo_encrypt_type]; 
		if (xfer && xfer->release)
			err = xfer->release(lo); 
		if (xfer && xfer->unlock)
			xfer->unlock(lo); 
		lo->lo_encrypt_type = 0;
	}
	return err;
}

static int loop_init_xfer(struct loop_device *lo, int type,struct loop_info *i)
{
	int err = 0; 
	if (type) {
		struct loop_func_table *xfer = xfer_funcs[type]; 
		if (xfer->init)
			err = xfer->init(lo, i);
		if (!err) { 
			lo->lo_encrypt_type = type;
			if (xfer->lock)
				xfer->lock(lo);
		}
	}
	return err;
}  

static int loop_clr_fd(struct loop_device *lo, struct block_device *bdev)
{
	struct file *filp = lo->lo_backing_file;
	int gfp = lo->old_gfp_mask;

	if (lo->lo_state != Lo_bound)
		return -ENXIO;
	if (lo->lo_refcnt > 1)	/* we needed one fd for the ioctl */
		return -EBUSY;
	if (filp==NULL)
		return -EINVAL;

	spin_lock_irq(&lo->lo_lock);
	lo->lo_state = Lo_rundown;
	if (atomic_dec_and_test(&lo->lo_pending))
		up(&lo->lo_bh_mutex);
	spin_unlock_irq(&lo->lo_lock);

	down(&lo->lo_sem);

	lo->lo_backing_file = NULL;

	loop_release_xfer(lo);
	lo->transfer = NULL;
	lo->ioctl = NULL;
	lo->lo_device = NULL;
	lo->lo_encrypt_type = 0;
	lo->lo_offset = 0;
	lo->lo_encrypt_key_size = 0;
	lo->lo_flags = 0;
	memset(lo->lo_encrypt_key, 0, LO_KEY_SIZE);
	memset(lo->lo_name, 0, LO_NAME_SIZE);
	loop_sizes[lo->lo_number] = 0;
	invalidate_bdev(bdev, 0);
	filp->f_dentry->d_inode->i_mapping->gfp_mask = gfp;
	lo->lo_state = Lo_unbound;
	fput(filp);
	MOD_DEC_USE_COUNT;
	return 0;
}

static int loop_set_status(struct loop_device *lo, struct loop_info *arg)
{
	struct loop_info info; 
	int err;
	unsigned int type;

	if (lo->lo_encrypt_key_size && lo->lo_key_owner != current->uid && 
	    !capable(CAP_SYS_ADMIN))
		return -EPERM;
	if (lo->lo_state != Lo_bound)
		return -ENXIO;
	if (copy_from_user(&info, arg, sizeof (struct loop_info)))
		return -EFAULT; 
	if ((unsigned int) info.lo_encrypt_key_size > LO_KEY_SIZE)
		return -EINVAL;
	type = info.lo_encrypt_type; 
	if (type >= MAX_LO_CRYPT || xfer_funcs[type] == NULL)
		return -EINVAL;
	if (type == LO_CRYPT_XOR && info.lo_encrypt_key_size == 0)
		return -EINVAL;
	err = loop_release_xfer(lo);
	if (!err) 
		err = loop_init_xfer(lo, type, &info);
	if (err)
		return err;	

	lo->lo_offset = info.lo_offset;
	strncpy(lo->lo_name, info.lo_name, LO_NAME_SIZE);

	lo->transfer = xfer_funcs[type]->transfer;
	lo->ioctl = xfer_funcs[type]->ioctl;
	lo->lo_encrypt_key_size = info.lo_encrypt_key_size;
	lo->lo_init[0] = info.lo_init[0];
	lo->lo_init[1] = info.lo_init[1];
	if (info.lo_encrypt_key_size) {
		memcpy(lo->lo_encrypt_key, info.lo_encrypt_key, 
		       info.lo_encrypt_key_size);
		lo->lo_key_owner = current->uid; 
	}	
	figure_loop_size(lo);
	return 0;
}

static int loop_get_status(struct loop_device *lo, struct loop_info *arg)
{
	struct loop_info	info;
	struct file *file = lo->lo_backing_file;

	if (lo->lo_state != Lo_bound)
		return -ENXIO;
	if (!arg)
		return -EINVAL;
	memset(&info, 0, sizeof(info));
	info.lo_number = lo->lo_number;
	info.lo_device = kdev_t_to_nr(file->f_dentry->d_inode->i_dev);
	info.lo_inode = file->f_dentry->d_inode->i_ino;
	info.lo_rdevice = lo->lo_device->bd_dev;
	info.lo_offset = lo->lo_offset;
	info.lo_flags = lo->lo_flags;
	strncpy(info.lo_name, lo->lo_name, LO_NAME_SIZE);
	info.lo_encrypt_type = lo->lo_encrypt_type;
	if (lo->lo_encrypt_key_size && capable(CAP_SYS_ADMIN)) {
		info.lo_encrypt_key_size = lo->lo_encrypt_key_size;
		memcpy(info.lo_encrypt_key, lo->lo_encrypt_key,
		       lo->lo_encrypt_key_size);
	}
	return copy_to_user(arg, &info, sizeof(info)) ? -EFAULT : 0;
}

static int lo_ioctl(struct inode * inode, struct file * file,
	unsigned int cmd, unsigned long arg)
{
	struct loop_device *lo;
	int dev, err;

	if (!inode)
		return -EINVAL;
	if (major(inode->i_rdev) != MAJOR_NR) {
		printk(KERN_WARNING "lo_ioctl: pseudo-major != %d\n",
		       MAJOR_NR);
		return -ENODEV;
	}
	dev = minor(inode->i_rdev);
	if (dev >= max_loop)
		return -ENODEV;
	lo = &loop_dev[dev];
	down(&lo->lo_ctl_mutex);
	switch (cmd) {
	case LOOP_SET_FD:
		err = loop_set_fd(lo, file, inode->i_bdev, arg);
		break;
	case LOOP_CLR_FD:
		err = loop_clr_fd(lo, inode->i_bdev);
		break;
	case LOOP_SET_STATUS:
		err = loop_set_status(lo, (struct loop_info *) arg);
		break;
	case LOOP_GET_STATUS:
		err = loop_get_status(lo, (struct loop_info *) arg);
		break;
	case BLKGETSIZE:
		if (lo->lo_state != Lo_bound) {
			err = -ENXIO;
			break;
		}
		err = put_user((unsigned long) loop_sizes[lo->lo_number] << 1, (unsigned long *) arg);
		break;
	case BLKGETSIZE64:
		if (lo->lo_state != Lo_bound) {
			err = -ENXIO;
			break;
		}
		err = put_user((u64)loop_sizes[lo->lo_number] << 10, (u64*)arg);
		break;
	case BLKBSZGET:
	case BLKBSZSET:
		err = blk_ioctl(inode->i_bdev, cmd, arg);
		break;
	default:
		err = lo->ioctl ? lo->ioctl(lo, cmd, arg) : -EINVAL;
	}
	up(&lo->lo_ctl_mutex);
	return err;
}

static int lo_open(struct inode *inode, struct file *file)
{
	struct loop_device *lo;
	int	dev, type;

	if (!inode)
		return -EINVAL;
	if (major(inode->i_rdev) != MAJOR_NR) {
		printk(KERN_WARNING "lo_open: pseudo-major != %d\n", MAJOR_NR);
		return -ENODEV;
	}
	dev = minor(inode->i_rdev);
	if (dev >= max_loop)
		return -ENODEV;

	lo = &loop_dev[dev];
	MOD_INC_USE_COUNT;
	down(&lo->lo_ctl_mutex);

	type = lo->lo_encrypt_type; 
	if (type && xfer_funcs[type] && xfer_funcs[type]->lock)
		xfer_funcs[type]->lock(lo);
	lo->lo_refcnt++;
	up(&lo->lo_ctl_mutex);
	return 0;
}

static int lo_release(struct inode *inode, struct file *file)
{
	struct loop_device *lo;
	int	dev, type;

	if (!inode)
		return 0;
	if (major(inode->i_rdev) != MAJOR_NR) {
		printk(KERN_WARNING "lo_release: pseudo-major != %d\n",
		       MAJOR_NR);
		return 0;
	}
	dev = minor(inode->i_rdev);
	if (dev >= max_loop)
		return 0;

	lo = &loop_dev[dev];
	down(&lo->lo_ctl_mutex);
	type = lo->lo_encrypt_type;
	--lo->lo_refcnt;
	if (xfer_funcs[type] && xfer_funcs[type]->unlock)
		xfer_funcs[type]->unlock(lo);

	up(&lo->lo_ctl_mutex);
	MOD_DEC_USE_COUNT;
	return 0;
}

static struct block_device_operations lo_fops = {
	owner:		THIS_MODULE,
	open:		lo_open,
	release:	lo_release,
	ioctl:		lo_ioctl,
};

/*
 * And now the modules code and kernel interface.
 */
MODULE_PARM(max_loop, "i");
MODULE_PARM_DESC(max_loop, "Maximum number of loop devices (1-256)");
MODULE_LICENSE("GPL");

int loop_register_transfer(struct loop_func_table *funcs)
{
	if ((unsigned)funcs->number > MAX_LO_CRYPT || xfer_funcs[funcs->number])
		return -EINVAL;
	xfer_funcs[funcs->number] = funcs;
	return 0; 
}

int loop_unregister_transfer(int number)
{
	struct loop_device *lo; 

	if ((unsigned)number >= MAX_LO_CRYPT)
		return -EINVAL; 
	for (lo = &loop_dev[0]; lo < &loop_dev[max_loop]; lo++) { 
		int type = lo->lo_encrypt_type;
		if (type == number) { 
			xfer_funcs[type]->release(lo);
			lo->transfer = NULL; 
			lo->lo_encrypt_type = 0; 
		}
	}
	xfer_funcs[number] = NULL; 
	return 0; 
}

EXPORT_SYMBOL(loop_register_transfer);
EXPORT_SYMBOL(loop_unregister_transfer);

int __init loop_init(void) 
{
	int	i;

	if ((max_loop < 1) || (max_loop > 256)) {
		printk(KERN_WARNING "loop: invalid max_loop (must be between"
				    " 1 and 256), using default (8)\n");
		max_loop = 8;
	}

	if (devfs_register_blkdev(MAJOR_NR, "loop", &lo_fops)) {
		printk(KERN_WARNING "Unable to get major number %d for loop"
				    " device\n", MAJOR_NR);
		return -EIO;
	}

	devfs_handle = devfs_mk_dir(NULL, "loop", NULL);
	devfs_register_series(devfs_handle, "%u", max_loop, DEVFS_FL_DEFAULT,
			      MAJOR_NR, 0,
			      S_IFBLK | S_IRUSR | S_IWUSR | S_IRGRP,
			      &lo_fops, NULL);

	loop_dev = kmalloc(max_loop * sizeof(struct loop_device), GFP_KERNEL);
	if (!loop_dev)
		return -ENOMEM;

	loop_sizes = kmalloc(max_loop * sizeof(int), GFP_KERNEL);
	if (!loop_sizes)
		goto out_mem;

	loop_blksizes = kmalloc(max_loop * sizeof(int), GFP_KERNEL);
	if (!loop_blksizes)
		goto out_mem;

	blk_queue_make_request(BLK_DEFAULT_QUEUE(MAJOR_NR), loop_make_request);
	blk_queue_bounce_limit(BLK_DEFAULT_QUEUE(MAJOR_NR), BLK_BOUNCE_HIGH);

	for (i = 0; i < max_loop; i++) {
		struct loop_device *lo = &loop_dev[i];
		memset(lo, 0, sizeof(struct loop_device));
		init_MUTEX(&lo->lo_ctl_mutex);
		init_MUTEX_LOCKED(&lo->lo_sem);
		init_MUTEX_LOCKED(&lo->lo_bh_mutex);
		lo->lo_number = i;
		spin_lock_init(&lo->lo_lock);
	}

	memset(loop_sizes, 0, max_loop * sizeof(int));
	memset(loop_blksizes, 0, max_loop * sizeof(int));
	blk_size[MAJOR_NR] = loop_sizes;
	blksize_size[MAJOR_NR] = loop_blksizes;
	for (i = 0; i < max_loop; i++)
		register_disk(NULL, mk_kdev(MAJOR_NR, i), 1, &lo_fops, 0);

	printk(KERN_INFO "loop: loaded (max %d devices)\n", max_loop);
	return 0;

out_mem:
	kfree(loop_dev);
	kfree(loop_sizes);
	printk(KERN_ERR "loop: ran out of memory\n");
	return -ENOMEM;
}

void loop_exit(void) 
{
	devfs_unregister(devfs_handle);
	if (devfs_unregister_blkdev(MAJOR_NR, "loop"))
		printk(KERN_WARNING "loop: cannot unregister blkdev\n");

	kfree(loop_dev);
	kfree(loop_sizes);
	kfree(loop_blksizes);
}

module_init(loop_init);
module_exit(loop_exit);

#ifndef MODULE
static int __init max_loop_setup(char *str)
{
	max_loop = simple_strtol(str, NULL, 0);
	return 1;
}

__setup("max_loop=", max_loop_setup);
#endif
