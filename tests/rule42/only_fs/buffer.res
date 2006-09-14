/*
 *  linux/fs/buffer.c
 *
 *  Copyright (C) 1991, 1992, 2002  Linus Torvalds
 */

/*
 * Start bdflush() with kernel_thread not syscall - Paul Gortmaker, 12/95
 *
 * Removed a lot of unnecessary code and simplified things now that
 * the buffer cache isn't our primary cache - Andrew Tridgell 12/96
 *
 * Speed up hash, lru, and free list operations.  Use gfp() for allocating
 * hash table, use SLAB cache for buffer heads. SMP threading.  -DaveM
 *
 * Added 32k buffer block sizes - these are required older ARM systems. - RMK
 *
 * async buffer flushing, 1999 Andrea Arcangeli <andrea@suse.de>
 */

#include <linux/config.h>
#include <linux/fs.h>
#include <linux/mm.h>
#include <linux/slab.h>
#include <linux/locks.h>
#include <linux/smp_lock.h>
#include <linux/blkdev.h>
#include <linux/file.h>
#include <linux/quotaops.h>
#include <linux/iobuf.h>
#include <linux/module.h>
#include <linux/writeback.h>
#include <linux/mempool.h>
#include <asm/bitops.h>

#define MAX_BUF_PER_PAGE (PAGE_CACHE_SIZE / 512)

#define BH_ENTRY(list) list_entry((list), struct buffer_head, b_inode_buffers)

/* This is used by some architectures to estimate available memory. */
atomic_t buffermem_pages = ATOMIC_INIT(0);

/*
 * Several of these buffer list functions are exported to filesystems,
 * so we do funny things with the spinlocking to support those
 * filesystems while still using inode->i_bufferlist_lock for
 * most applications.
 * FIXME: put a spinlock in the reiserfs journal and kill this lock.
 */
static spinlock_t global_bufferlist_lock = SPIN_LOCK_UNLOCKED;

/*
 * Debug/devel support stuff
 */

void __buffer_error(char *file, int line)
{
	static int enough;

	if (enough > 10)
		return;
	enough++;
	printk("buffer layer error at %s:%d\n", file, line);
#ifdef CONFIG_X86
	printk("Pass this trace through ksymoops for reporting\n");
	{
		extern void show_stack(long *esp);
		show_stack(0);
	}
#endif
}
EXPORT_SYMBOL(__buffer_error);

inline void
init_buffer(struct buffer_head *bh, bh_end_io_t *handler, void *private)
{
	bh->b_end_io = handler;
	bh->b_private = private;
}

void unlock_buffer(struct buffer_head *bh)
{
	/*
	 * unlock_buffer against a zero-count bh is a bug, if the page
	 * is not locked.  Because then nothing protects the buffer's
	 * waitqueue, which is used here. (Well.  Other locked buffers
	 * against the page will pin it.  But complain anyway).
	 */
	if (atomic_read(&bh->b_count) == 0 && !PageLocked(bh->b_page))
		buffer_error();

	clear_bit(BH_Lock, &bh->b_state);
	smp_mb__after_clear_bit();
	if (waitqueue_active(&bh->b_wait))
		wake_up(&bh->b_wait);
}

static inline void
__set_page_buffers(struct page *page, struct buffer_head *head)
{
	struct inode *inode = page->mapping->host;

	if (inode && S_ISBLK(inode->i_mode))
		atomic_inc(&buffermem_pages);
	if (page_has_buffers(page))
		buffer_error();
	set_page_buffers(page, head);
	page_cache_get(page);
}

static inline void
__clear_page_buffers(struct page *page)
{
	struct address_space *mapping = page->mapping;

	if (mapping) {
		struct inode *inode = mapping->host;

		if (S_ISBLK(inode->i_mode))
			atomic_dec(&buffermem_pages);
	}
	clear_page_buffers(page);
	page_cache_release(page);
}

/*
 * Block until a buffer comes unlocked.  This doesn't stop it
 * from becoming locked again - you have to lock it yourself
 * if you want to preserve its state.
 */
void __wait_on_buffer(struct buffer_head * bh)
{
	struct task_struct *tsk = current;
	DECLARE_WAITQUEUE(wait, tsk);

	get_bh(bh);
	add_wait_queue(&bh->b_wait, &wait);
	do {
		run_task_queue(&tq_disk);
		set_task_state(tsk, TASK_UNINTERRUPTIBLE);
		if (!buffer_locked(bh))
			break;
		schedule();
	} while (buffer_locked(bh));
	tsk->state = TASK_RUNNING;
	remove_wait_queue(&bh->b_wait, &wait);
	put_bh(bh);
}

/*
 * Default synchronous end-of-IO handler..  Just mark it up-to-date and
 * unlock the buffer. This is what ll_rw_block uses too.
 */
void end_buffer_io_sync(struct buffer_head *bh, int uptodate)
{
	if (!uptodate)
		printk("%s: I/O error\n", __FUNCTION__);
	mark_buffer_uptodate(bh, uptodate);
	unlock_buffer(bh);
	put_bh(bh);
}

/*
 * write out all the dirty data associated with a block device
 * via its mapping.  Does not take the superblock lock.
 *
 * If `wait' is true, wait on the writeout.
 */
int sync_buffers(struct block_device *bdev, int wait)
{
	int ret;

	ret = filemap_fdatasync(bdev->bd_inode->i_mapping);
	if (wait) {
		int err;

		err = filemap_fdatawait(bdev->bd_inode->i_mapping);
		if (!ret)
			ret = err;
	}
	return ret;
}

/*
 * Write out all the dirty data associated with a block device
 * via its mapping.  Does not take the superblock lock.
 *
 * Wait on the writeout.
 */
int fsync_no_super(struct block_device *bdev)
{
	int ret = 0;

	if (bdev)
		ret = sync_buffers(bdev, 1);
	return ret;
}

/*
 * Write out and wait upon all dirty data associated with this
 * superblock.  Filesystem data as well as the underlying block
 * device.  Takes the superblock lock.
 */
int fsync_super(struct super_block *sb)
{
	sync_inodes_sb(sb);	/* All the inodes */
	DQUOT_SYNC(sb);
	lock_super(sb);
	if (sb->s_dirt && sb->s_op && sb->s_op->write_super)
		sb->s_op->write_super(sb);
	unlock_super(sb);

	return fsync_no_super(sb->s_bdev);
}

/*
 * Write out and wait upon all dirty data associated with this
 * device.   Filesystem data as well as the underlying block
 * device.  Takes the superblock lock.
 */
int fsync_bdev(struct block_device *bdev)
{
	struct super_block *sb = get_super(to_kdev_t(bdev->bd_dev));
	if (sb) {
		int res = fsync_super(sb);
		drop_super(sb);
		return res;
	}
	return fsync_no_super(bdev);
}

/*
 * Write out and wait upon all dirty data associated with this
 * kdev_t.   Filesystem data as well as the underlying block
 * device.  Takes the superblock lock.
 */
int fsync_dev(kdev_t dev)
{
	struct block_device *bdev = bdget(kdev_t_to_nr(dev));
	if (bdev) {
		int res = fsync_bdev(bdev);
		bdput(bdev);
		return res;
	}
	return 0;
}

/*
 * sync everything.
 */
asmlinkage long sys_sync(void)
{
	sync_inodes();	/* All mappings and inodes, including block devices */
	DQUOT_SYNC(NULL);
	sync_supers();	/* Write the superblocks */
	sync_inodes();	/* All the mappings and inodes, again. */
	return 0;
}

/*
 * Generic function to fsync a file.
 *
 * filp may be NULL if called via the msync of a vma.
 */
 
int file_fsync(struct file *filp, struct dentry *dentry, int datasync)
{
	struct inode * inode = dentry->d_inode;
	struct super_block * sb;
	int ret;

	/* sync the inode to buffers */
	write_inode_now(inode, 0);

	/* sync the superblock to buffers */
	sb = inode->i_sb;
	lock_super(sb);
	if (sb->s_op && sb->s_op->write_super)
		sb->s_op->write_super(sb);
	unlock_super(sb);

	/* .. finally sync the buffers to disk */
	ret = sync_buffers(sb->s_bdev, 1);
	return ret;
}

asmlinkage long sys_fsync(unsigned int fd)
{
	struct file * file;
	struct dentry * dentry;
	struct inode * inode;
	int ret, err;

	ret = -EBADF;
	file = fget(fd);
	if (!file)
		goto out;

	dentry = file->f_dentry;
	inode = dentry->d_inode;

	ret = -EINVAL;
	if (!file->f_op || !file->f_op->fsync) {
		/* Why?  We can still call filemap_fdatasync */
		goto out_putf;
	}

	/* We need to protect against concurrent writers.. */
	down(&inode->i_sem);
	ret = filemap_fdatasync(inode->i_mapping);
	err = file->f_op->fsync(file, dentry, 0);
	if (err && !ret)
		ret = err;
	err = filemap_fdatawait(inode->i_mapping);
	if (err && !ret)
		ret = err;
	up(&inode->i_sem);

out_putf:
	fput(file);
out:
	return ret;
}

asmlinkage long sys_fdatasync(unsigned int fd)
{
	struct file * file;
	struct dentry * dentry;
	struct inode * inode;
	int ret, err;

	ret = -EBADF;
	file = fget(fd);
	if (!file)
		goto out;

	dentry = file->f_dentry;
	inode = dentry->d_inode;

	ret = -EINVAL;
	if (!file->f_op || !file->f_op->fsync)
		goto out_putf;

	down(&inode->i_sem);
	ret = filemap_fdatasync(inode->i_mapping);
	err = file->f_op->fsync(file, dentry, 1);
	if (err && !ret)
		ret = err;
	err = filemap_fdatawait(inode->i_mapping);
	if (err && !ret)
		ret = err;
	up(&inode->i_sem);

out_putf:
	fput(file);
out:
	return ret;
}

/*
 * Various filesystems appear to want __get_hash_table to be non-blocking.
 * But it's the page lock which protects the buffers.  To get around this,
 * we get exclusion from try_to_free_buffers with the inode's
 * i_bufferlist_lock.
 *
 * Hack idea: for the blockdev mapping, i_bufferlist_lock contention
 * may be quite high.  This code could TryLock the page, and if that
 * succeeds, there is no need to take i_bufferlist_lock. (But if
 * i_bufferlist_lock is contended then so is mapping->page_lock).
 */
struct buffer_head *
__get_hash_table(struct block_device *bdev, sector_t block, int unused)
{
	struct inode * const inode = bdev->bd_inode;
	struct buffer_head *ret = NULL;
	unsigned long index;
	struct buffer_head *bh;
	struct buffer_head *head;
	struct page *page;

	index = block >> (PAGE_CACHE_SHIFT - inode->i_blkbits);
	page = find_get_page(inode->i_mapping, index);
	if (!page)
		goto out;

	spin_lock(&inode->i_bufferlist_lock);
	if (!page_has_buffers(page))
		goto out_unlock;
	head = page_buffers(page);
	bh = head;
	do {
		if (bh->b_blocknr == block) {
			ret = bh;
			get_bh(bh);
			goto out_unlock;
		}
		bh = bh->b_this_page;
	} while (bh != head);
	buffer_error();
out_unlock:
	spin_unlock(&inode->i_bufferlist_lock);
	page_cache_release(page);
out:
	return ret;
}

void buffer_insert_list(spinlock_t *lock,
		struct buffer_head *bh, struct list_head *list)
{
	if (lock == NULL)
		lock = &global_bufferlist_lock;
	spin_lock(lock);
	if (bh->b_inode)
		list_del(&bh->b_inode_buffers);
	bh->b_inode = 1;
	list_add(&bh->b_inode_buffers, list);
	spin_unlock(lock);
}

/*
 * i_bufferlist_lock must be held
 */
static inline void __remove_inode_queue(struct buffer_head *bh)
{
	if (bh->b_inode) {
		list_del(&bh->b_inode_buffers);
		bh->b_inode = 0;
	}
}

int inode_has_buffers(struct inode *inode)
{
	int ret;
	
	spin_lock(&inode->i_bufferlist_lock);
	ret = !list_empty(&inode->i_dirty_buffers) ||
			!list_empty(&inode->i_dirty_data_buffers);
	spin_unlock(&inode->i_bufferlist_lock);
	
	return ret;
}

/* If invalidate_buffers() will trash dirty buffers, it means some kind
   of fs corruption is going on. Trashing dirty data always imply losing
   information that was supposed to be just stored on the physical layer
   by the user.

   Thus invalidate_buffers in general usage is not allwowed to trash
   dirty buffers. For example ioctl(FLSBLKBUF) expects dirty data to
   be preserved.  These buffers are simply skipped.
  
   We also skip buffers which are still in use.  For example this can
   happen if a userspace program is reading the block device.

   NOTE: In the case where the user removed a removable-media-disk even if
   there's still dirty data not synced on disk (due a bug in the device driver
   or due an error of the user), by not destroying the dirty buffers we could
   generate corruption also on the next media inserted, thus a parameter is
   necessary to handle this case in the most safe way possible (trying
   to not corrupt also the new disk inserted with the data belonging to
   the old now corrupted disk). Also for the ramdisk the natural thing
   to do in order to release the ramdisk memory is to destroy dirty buffers.

   These are two special cases. Normal usage imply the device driver
   to issue a sync on the device (without waiting I/O completion) and
   then an invalidate_buffers call that doesn't trash dirty buffers.

   For handling cache coherency with the blkdev pagecache the 'update' case
   is been introduced. It is needed to re-read from disk any pinned
   buffer. NOTE: re-reading from disk is destructive so we can do it only
   when we assume nobody is changing the buffercache under our I/O and when
   we think the disk contains more recent information than the buffercache.
   The update == 1 pass marks the buffers we need to update, the update == 2
   pass does the actual I/O. */
void invalidate_bdev(struct block_device *bdev, int destroy_dirty_buffers)
{
	/*
	 * FIXME: what about destroy_dirty_buffers?
	 * We really want to use invalidate_inode_pages2() for
	 * that, but not until that's cleaned up.
	 */
	invalidate_inode_pages(bdev->bd_inode);
}

void __invalidate_buffers(kdev_t dev, int destroy_dirty_buffers)
{
	struct block_device *bdev = bdget(kdev_t_to_nr(dev));
	if (bdev) {
		invalidate_bdev(bdev, destroy_dirty_buffers);
		bdput(bdev);
	}
}

/*
 * FIXME: What is this function actually trying to do?  Why "zones[0]"?
 * Is it still correct/needed if/when blockdev mappings use GFP_HIGHUSER?
 */
static void free_more_memory(void)
{
	zone_t *zone;

	zone = contig_page_data.node_zonelists[GFP_NOFS & GFP_ZONEMASK].zones[0];

	wakeup_bdflush();
	try_to_free_pages(zone, GFP_NOFS, 0);
	run_task_queue(&tq_disk);
	__set_current_state(TASK_RUNNING);
	yield();
}

static void end_buffer_io_async(struct buffer_head *bh, int uptodate)
{
	static spinlock_t page_uptodate_lock = SPIN_LOCK_UNLOCKED;
	unsigned long flags;
	struct buffer_head *tmp;
	struct page *page;
	int page_uptodate = 1;

	if (!uptodate)
		printk("%s: I/O error\n", __FUNCTION__);

	mark_buffer_uptodate(bh, uptodate);
	page = bh->b_page;
	if (!uptodate)
		SetPageError(page);

	/*
	 * Be _very_ careful from here on. Bad things can happen if
	 * two buffer heads end IO at almost the same time and both
	 * decide that the page is now completely done.
	 */
	spin_lock_irqsave(&page_uptodate_lock, flags);
	mark_buffer_async(bh, 0);
	unlock_buffer(bh);
	tmp = bh;
	do {
		if (!buffer_uptodate(tmp))
			page_uptodate = 0;
		if (buffer_async(tmp)) {
			if (buffer_locked(tmp))
				goto still_busy;
			if (!buffer_mapped(bh))
				BUG();
		}
		tmp = tmp->b_this_page;
	} while (tmp != bh);
	spin_unlock_irqrestore(&page_uptodate_lock, flags);

	/*
	 * If none of the buffers had errors and they are all
	 * uptodate then we can set the page uptodate.
	 */
	if (page_uptodate && !PageError(page))
		SetPageUptodate(page);
	unlock_page(page);
	return;

still_busy:
	spin_unlock_irqrestore(&page_uptodate_lock, flags);
	return;
}

/*
 * If a page's buffers are under async writeout (end_buffer_io_async
 * completion) then there is a possibility that another thread of
 * control could lock one of the buffers after it has completed
 * but while some of the other buffers have not completed.  This
 * locked buffer would confuse end_buffer_io_async() into not unlocking
 * the page.  So the absence of BH_Async tells end_buffer_io_async()
 * that this buffer is not under async I/O.
 *
 * The page comes unlocked when it has no locked buffer_async buffers
 * left.
 *
 * The page lock prevents anyone starting new async I/O against any of
 * the buffers.
 */
inline void set_buffer_async_io(struct buffer_head *bh)
{
	bh->b_end_io = end_buffer_io_async;
	mark_buffer_async(bh, 1);
}

/*
 * osync is designed to support O_SYNC io.  It waits synchronously for
 * all already-submitted IO to complete, but does not queue any new
 * writes to the disk.
 *
 * To do O_SYNC writes, just queue the buffer writes with ll_rw_block as
 * you dirty the buffers, and then use osync_inode_buffers to wait for
 * completion.  Any other dirty buffers which are not yet queued for
 * write will not be flushed to disk by the osync.
 */
static int osync_buffers_list(spinlock_t *lock, struct list_head *list)
{
	struct buffer_head *bh;
	struct list_head *p;
	int err = 0;

	if (lock == NULL)
		lock = &global_bufferlist_lock;

	spin_lock(lock);
repeat:
	for (p = list->prev; 
	     bh = BH_ENTRY(p), p != list;
	     p = bh->b_inode_buffers.prev) {
		if (buffer_locked(bh)) {
			get_bh(bh);
			spin_unlock(lock);
			wait_on_buffer(bh);
			if (!buffer_uptodate(bh))
				err = -EIO;
			brelse(bh);
			spin_lock(lock);
			goto repeat;
		}
	}
	spin_unlock(lock);
	return err;
}

/*
 * Synchronise all the inode's dirty buffers to the disk.
 *
 * We have conflicting pressures: we want to make sure that all
 * initially dirty buffers get waited on, but that any subsequently
 * dirtied buffers don't.  After all, we don't want fsync to last
 * forever if somebody is actively writing to the file.
 *
 * Do this in two main stages: first we copy dirty buffers to a
 * temporary inode list, queueing the writes as we go.  Then we clean
 * up, waiting for those writes to complete.
 * 
 * During this second stage, any subsequent updates to the file may end
 * up refiling the buffer on the original inode's dirty list again, so
 * there is a chance we will end up with a buffer queued for write but
 * not yet completed on that list.  So, as a final cleanup we go through
 * the osync code to catch these locked, dirty buffers without requeuing
 * any newly dirty buffers for write.
 */
int fsync_buffers_list(spinlock_t *lock, struct list_head *list)
{
	struct buffer_head *bh;
	struct list_head tmp;
	int err = 0, err2;

	if (lock == NULL)
		lock = &global_bufferlist_lock;
	
	INIT_LIST_HEAD(&tmp);

	spin_lock(lock);
	while (!list_empty(list)) {
		bh = BH_ENTRY(list->next);
		list_del(&bh->b_inode_buffers);
		if (!buffer_dirty(bh) && !buffer_locked(bh))
			bh->b_inode = 0;
		else {
			bh->b_inode = 1;
			list_add(&bh->b_inode_buffers, &tmp);
			if (buffer_dirty(bh)) {
				get_bh(bh);
				spin_unlock(lock);
				ll_rw_block(WRITE, 1, &bh);
				brelse(bh);
				spin_lock(lock);
			}
		}
	}

	while (!list_empty(&tmp)) {
		bh = BH_ENTRY(tmp.prev);
		__remove_inode_queue(bh);
		get_bh(bh);
		spin_unlock(lock);
		wait_on_buffer(bh);
		if (!buffer_uptodate(bh))
			err = -EIO;
		brelse(bh);
		spin_lock(lock);
	}
	
	spin_unlock(lock);
	err2 = osync_buffers_list(lock, list);
	if (err)
		return err;
	else
		return err2;
}

/*
 * Invalidate any and all dirty buffers on a given inode.  We are
 * probably unmounting the fs, but that doesn't mean we have already
 * done a sync().  Just drop the buffers from the inode list.
 */
void invalidate_inode_buffers(struct inode *inode)
{
	struct list_head * entry;
	
	spin_lock(&inode->i_bufferlist_lock);
	while ((entry = inode->i_dirty_buffers.next) !=
				&inode->i_dirty_buffers)
		__remove_inode_queue(BH_ENTRY(entry));
	while ((entry = inode->i_dirty_data_buffers.next) !=
				&inode->i_dirty_data_buffers)
		__remove_inode_queue(BH_ENTRY(entry));
	spin_unlock(&inode->i_bufferlist_lock);
}

/*
 * Create the appropriate buffers when given a page for data area and
 * the size of each buffer.. Use the bh->b_this_page linked list to
 * follow the buffers created.  Return NULL if unable to create more
 * buffers.
 * The async flag is used to differentiate async IO (paging, swapping)
 * from ordinary buffer allocations, and only async requests are allowed
 * to sleep waiting for buffer heads. 
 */
static struct buffer_head *
create_buffers(struct page * page, unsigned long size, int async)
{
	struct buffer_head *bh, *head;
	long offset;

try_again:
	head = NULL;
	offset = PAGE_SIZE;
	while ((offset -= size) >= 0) {
		bh = alloc_buffer_head(async);
		if (!bh)
			goto no_grow;

		bh->b_bdev = NULL;
		bh->b_this_page = head;
		head = bh;

		bh->b_state = 0;
		atomic_set(&bh->b_count, 0);
		bh->b_size = size;

		/* Link the buffer to its page */
		set_bh_page(bh, page, offset);

		bh->b_end_io = NULL;
	}
	return head;
/*
 * In case anything failed, we just free everything we got.
 */
no_grow:
	if (head) {
		do {
			bh = head;
			head = head->b_this_page;
			free_buffer_head(bh);
		} while (head);
	}

	/*
	 * Return failure for non-async IO requests.  Async IO requests
	 * are not allowed to fail, so we have to wait until buffer heads
	 * become available.  But we don't want tasks sleeping with 
	 * partially complete buffers, so all were released above.
	 */
	if (!async)
		return NULL;

	/* We're _really_ low on memory. Now we just
	 * wait for old buffer heads to become free due to
	 * finishing IO.  Since this is an async request and
	 * the reserve list is empty, we're sure there are 
	 * async buffer heads in use.
	 */
	run_task_queue(&tq_disk);

	free_more_memory();
	goto try_again;
}

static inline void
link_dev_buffers(struct page *page, struct buffer_head *head)
{
	struct buffer_head *bh, *tail;

	bh = head;
	do {
		tail = bh;
		bh = bh->b_this_page;
	} while (bh);
	tail->b_this_page = head;
	__set_page_buffers(page, head);
}

/*
 * Initialise the state of a blockdev page's buffers.
 */ 
static /*inline*/ void
init_page_buffers(struct page *page, struct block_device *bdev,
			int block, int size)
{
	struct buffer_head *head = page_buffers(page);
	struct buffer_head *bh = head;
	unsigned int b_state;

	b_state = 1 << BH_Mapped;
	if (PageUptodate(page))
		b_state |= 1 << BH_Uptodate;

	do {
		if (!(bh->b_state & (1 << BH_Mapped))) {
			init_buffer(bh, NULL, NULL);
			bh->b_bdev = bdev;
			bh->b_blocknr = block;
			bh->b_state = b_state;
		}
		block++;
		bh = bh->b_this_page;
	} while (bh != head);
}

/*
 * Create the page-cache page that contains the requested block.
 *
 * This is user purely for blockdev mappings.
 */
static /*inline*/ struct page *
grow_dev_page(struct block_device *bdev, unsigned long block,
			unsigned long index, int size)
{
	struct inode *inode = bdev->bd_inode;
	struct page *page;
	struct buffer_head *bh;

	page = find_or_create_page(inode->i_mapping, index, GFP_NOFS);
	if (!page)
		return NULL;

	if (!PageLocked(page))
		BUG();

	if (page_has_buffers(page)) {
		bh = page_buffers(page);
		if (bh->b_size == size)
			return page;
		if (!try_to_free_buffers(page))
			goto failed;
	}

	/*
	 * Allocate some buffers for this page
	 */
	bh = create_buffers(page, size, 0);
	if (!bh)
		goto failed;

	/*
	 * Link the page to the buffers and initialise them.  Take the
	 * lock to be atomic wrt __get_hash_table(), which does not
	 * run under the page lock.
	 */
	spin_lock(&inode->i_bufferlist_lock);
	link_dev_buffers(page, bh);
	init_page_buffers(page, bdev, block, size);
	spin_unlock(&inode->i_bufferlist_lock);
	return page;

failed:
	buffer_error();
	unlock_page(page);
	page_cache_release(page);
	return NULL;
}

/*
 * Create buffers for the specified block device block's page.  If
 * that page was dirty, the buffers are set dirty also.
 *
 * Except that's a bug.  Attaching dirty buffers to a dirty
 * blockdev's page can result in filesystem corruption, because
 * some of those buffers may be aliases of filesystem data.
 * grow_dev_page() will go BUG() if this happens.
 */
static inline int
grow_buffers(struct block_device *bdev, unsigned long block, int size)
{
	struct page *page;
	unsigned long index;
	int sizebits;

	/* Size must be multiple of hard sectorsize */
	if (size & (bdev_hardsect_size(bdev)-1))
		BUG();
	if (size < 512 || size > PAGE_SIZE)
		BUG();

	sizebits = -1;
	do {
		sizebits++;
	} while ((size << sizebits) < PAGE_SIZE);

	index = block >> sizebits;
	block = index << sizebits;

	/* Create a page with the proper size buffers.. */
	page = grow_dev_page(bdev, block, index, size);
	if (!page)
		return 0;
	unlock_page(page);
	page_cache_release(page);
	return 1;
}

/*
 * __getblk will locate (and, if necessary, create) the buffer_head
 * which corresponds to the passed block_device, block and size. The
 * returned buffer has its reference count incremented.
 *
 * __getblk() cannot fail - it just keeps trying.  If you pass it an
 * illegal block number, __getblk() will happily return a buffer_head
 * which represents the non-existent block.  Very weird.
 *
 * __getblk() will lock up the machine if grow_dev_page's try_to_free_buffers()
 * attempt is failing.  FIXME, perhaps?
 */
struct buffer_head *
__getblk(struct block_device *bdev, sector_t block, int size)
{
	for (;;) {
		struct buffer_head * bh;

		bh = __get_hash_table(bdev, block, size);
		if (bh) {
			touch_buffer(bh);
			return bh;
		}

		if (!grow_buffers(bdev, block, size))
			free_more_memory();
	}
}

/*
 * The relationship between dirty buffers and dirty pages:
 *
 * Whenever a page has any dirty buffers, the page's dirty bit is set, and
 * the page appears on its address_space.dirty_pages list.
 *
 * At all times, the dirtiness of the buffers represents the dirtiness of
 * subsections of the page.  If the page has buffers, the page dirty bit is
 * merely a hint about the true dirty state.
 *
 * When a page is set dirty in its entirety, all its buffers are marked dirty
 * (if the page has buffers).
 *
 * When a buffer is marked dirty, its page is dirtied, but the page's other
 * buffers are not.
 *
 * Also.  When blockdev buffers are explicitly read with bread(), they
 * individually become uptodate.  But their backing page remains not
 * uptodate - even if all of its buffers are uptodate.  A subsequent
 * block_read_full_page() against that page will discover all the uptodate
 * buffers, will set the page uptodate and will perform no I/O.
 */
static inline void __mark_dirty(struct buffer_head *bh)
{
	__set_page_dirty_nobuffers(bh->b_page);
}

/**
 * mark_buffer_dirty - mark a buffer_head as needing writeout
 *
 * mark_buffer_dirty() will set the dirty bit against the buffer,
 * then set its backing page dirty, then attach the page to its
 * address_space's dirty_pages list and then attach the address_space's
 * inode to its superblock's dirty inode list.
 *
 * mark_buffer_dirty() is atomic.  It takes inode->i_bufferlist_lock,
 * mapping->page_lock and the global inode_lock.
 */
void mark_buffer_dirty(struct buffer_head *bh)
{
	if (!atomic_set_buffer_dirty(bh))
		__mark_dirty(bh);
}

/*
 * Decrement a buffer_head's reference count.  If all buffers against a page
 * have zero reference count, are clean and unlocked, and if the page is clean
 * and unlocked then try_to_free_buffers() may strip the buffers from the page
 * in preparation for freeing it (sometimes, rarely, buffers are removed from
 * a page but it ends up not being freed, and buffers may later be reattached).
 */
void __brelse(struct buffer_head * buf)
{
	if (atomic_read(&buf->b_count)) {
		put_bh(buf);
		return;
	}
	printk(KERN_ERR "VFS: brelse: Trying to free free buffer\n");
	buffer_error();		/* For the stack backtrace */
}

/*
 * bforget() is like brelse(), except it discards any
 * potentially dirty data.
 */
void __bforget(struct buffer_head * buf)
{
	mark_buffer_clean(buf);
	__brelse(buf);
}

/**
 *  bread() - reads a specified block and returns the bh
 *  @block: number of block
 *  @size: size (in bytes) to read
 * 
 *  Reads a specified block, and returns buffer head that contains it.
 *  It returns NULL if the block was unreadable.
 */
struct buffer_head * __bread(struct block_device *bdev, int block, int size)
{
	struct buffer_head *bh = __getblk(bdev, block, size);

	if (buffer_uptodate(bh))
		return bh;
	lock_buffer(bh);
	if (buffer_uptodate(bh)) {
		unlock_buffer(bh);
		return bh;
	} else {
		if (buffer_dirty(bh))
			buffer_error();
		get_bh(bh);
		bh->b_end_io = end_buffer_io_sync;
		submit_bh(READ, bh);
		wait_on_buffer(bh);
		if (buffer_uptodate(bh))
			return bh;
	}
	brelse(bh);
	return NULL;
}

void set_bh_page(struct buffer_head *bh,
		struct page *page, unsigned long offset)
{
	bh->b_page = page;
	if (offset >= PAGE_SIZE)
		BUG();
	if (PageHighMem(page))
		/*
		 * This catches illegal uses and preserves the offset:
		 */
		bh->b_data = (char *)(0 + offset);
	else
		bh->b_data = page_address(page) + offset;
}
EXPORT_SYMBOL(set_bh_page);

/*
 * Called when truncating a buffer on a page completely.
 */
static void discard_buffer(struct buffer_head * bh)
{
	if (buffer_mapped(bh)) {
		mark_buffer_clean(bh);
		lock_buffer(bh);
		bh->b_bdev = NULL;
		clear_bit(BH_Mapped, &bh->b_state);
		clear_bit(BH_Req, &bh->b_state);
		clear_bit(BH_New, &bh->b_state);
		unlock_buffer(bh);
	}
}

/**
 * try_to_release_page() - release old fs-specific metadata on a page
 *
 * @page: the page which the kernel is trying to free
 * @gfp_mask: memory allocation flags (and I/O mode)
 *
 * The address_space is to try to release any data against the page
 * (presumably at page->private).  If the release was successful, return `1'.
 * Otherwise return zero.
 *
 * The @gfp_mask argument specifies whether I/O may be performed to release
 * this page (__GFP_IO), and whether the call may block (__GFP_WAIT).
 *
 * NOTE: @gfp_mask may go away, and this function may become non-blocking.
 */
int try_to_release_page(struct page *page, int gfp_mask)
{
	struct address_space * const mapping = page->mapping;

	if (!PageLocked(page))
		BUG();
	
	if (mapping && mapping->a_ops->releasepage)
		return mapping->a_ops->releasepage(page, gfp_mask);
	return try_to_free_buffers(page);
}

/**
 * block_flushpage - invalidate part of all of a buffer-backed page
 *
 * @page: the page which is affected
 * @offset: the index of the truncation point
 *
 * block_flushpage() should be called block_invalidatepage().  It is
 * called when all or part of the page has become invalidatedby a truncate
 * operation.
 *
 * block_flushpage() does not have to release all buffers, but it must
 * ensure that no dirty buffer is left outside @offset and that no I/O
 * is underway against any of the blocks which are outside the truncation
 * point.  Because the caller is about to free (and possibly reuse) those
 * blocks on-disk.
 */
int block_flushpage(struct page *page, unsigned long offset)
{
	struct buffer_head *head, *bh, *next;
	unsigned int curr_off = 0;

	if (!PageLocked(page))
		BUG();
	if (!page_has_buffers(page))
		return 1;

	head = page_buffers(page);
	bh = head;
	do {
		unsigned int next_off = curr_off + bh->b_size;
		next = bh->b_this_page;

		/*
		 * is this block fully invalidated?
		 */
		if (offset <= curr_off)
			discard_buffer(bh);
		curr_off = next_off;
		bh = next;
	} while (bh != head);

	/*
	 * We release buffers only if the entire page is being invalidated.
	 * The get_block cached value has been unconditionally invalidated,
	 * so real IO is not possible anymore.
	 */
	if (offset == 0) {
		if (!try_to_release_page(page, 0))
			return 0;
	}

	return 1;
}

/*
 * We attach and possibly dirty the buffers atomically wrt
 * __set_page_dirty_buffers() via i_bufferlist_lock.  try_to_free_buffers
 * is already excluded via the page lock.
 */
void create_empty_buffers(struct page *page,
			unsigned long blocksize, unsigned long b_state)
{
	struct buffer_head *bh, *head, *tail;

	head = create_buffers(page, blocksize, 1);
	bh = head;
	do {
		bh->b_end_io = NULL;
		bh->b_state |= b_state;
		tail = bh;
		bh = bh->b_this_page;
	} while (bh);
	tail->b_this_page = head;

	spin_lock(&page->mapping->host->i_bufferlist_lock);
	if (PageDirty(page)) {
		bh = head;
		do {
			set_bit(BH_Dirty, &bh->b_state);
			bh = bh->b_this_page;
		} while (bh != head);
	}
	__set_page_buffers(page, head);
	spin_unlock(&page->mapping->host->i_bufferlist_lock);
}
EXPORT_SYMBOL(create_empty_buffers);

/*
 * We are taking a block for data and we don't want any output from any
 * buffer-cache aliases starting from return from that function and
 * until the moment when something will explicitly mark the buffer
 * dirty (hopefully that will not happen until we will free that block ;-)
 * We don't even need to mark it not-uptodate - nobody can expect
 * anything from a newly allocated buffer anyway. We used to used
 * unmap_buffer() for such invalidation, but that was wrong. We definitely
 * don't want to mark the alias unmapped, for example - it would confuse
 * anyone who might pick it with bread() afterwards...
 */
static void unmap_underlying_metadata(struct buffer_head *bh)
{
	struct buffer_head *old_bh;

	old_bh = __get_hash_table(bh->b_bdev, bh->b_blocknr, 0);
	if (old_bh) {
#if 0	/* This happens.  Later. */
		if (buffer_dirty(old_bh))
			buffer_error();
#endif
		mark_buffer_clean(old_bh);
		wait_on_buffer(old_bh);
		clear_bit(BH_Req, &old_bh->b_state);
		__brelse(old_bh);
	}
}

/*
 * NOTE! All mapped/uptodate combinations are valid:
 *
 *	Mapped	Uptodate	Meaning
 *
 *	No	No		"unknown" - must do get_block()
 *	No	Yes		"hole" - zero-filled
 *	Yes	No		"allocated" - allocated on disk, not read in
 *	Yes	Yes		"valid" - allocated and up-to-date in memory.
 *
 * "Dirty" is valid only with the last case (mapped+uptodate).
 */

/*
 * While block_write_full_page is writing back the dirty buffers under
 * the page lock, whoever dirtied the buffers may decide to clean them
 * again at any time.  We handle that by only looking at the buffer
 * state inside lock_buffer().
 */
static int __block_write_full_page(struct inode *inode,
			struct page *page, get_block_t *get_block)
{
	int err;
	unsigned long block;
	unsigned long last_block;
	struct buffer_head *bh, *head;
	int nr_underway = 0;

	if (!PageLocked(page))
		BUG();

	last_block = (inode->i_size - 1) >> inode->i_blkbits;

	if (!page_has_buffers(page)) {
		if (S_ISBLK(inode->i_mode))
			buffer_error();
		if (!PageUptodate(page))
			buffer_error();
		create_empty_buffers(page, 1 << inode->i_blkbits,
					(1 << BH_Dirty)|(1 << BH_Uptodate));
	}

	block = page->index << (PAGE_CACHE_SHIFT - inode->i_blkbits);
	head = page_buffers(page);
	bh = head;

	/*
	 * Get all the dirty buffers mapped to disk addresses and
	 * handle any aliases from the underlying blockdev's mapping.
	 */
	do {
		if (block > last_block) {
			if (buffer_dirty(bh))
				buffer_error();
			if (buffer_mapped(bh))
				buffer_error();
			/*
			 * NOTE: this buffer can only be marked uptodate
			 * because we know that block_write_full_page has
			 * zeroed it out.  That seems unnecessary and may go
			 * away.
			 */
			mark_buffer_uptodate(bh, 1);
		} else if (!buffer_mapped(bh) && buffer_dirty(bh)) {
			if (buffer_new(bh))
				buffer_error();
			err = get_block(inode, block, bh, 1);
			if (err)
				goto recover;
			if (buffer_new(bh)) {
				/* blockdev mappings never come here */
				clear_bit(BH_New, &bh->b_state);
				unmap_underlying_metadata(bh);
			}
		}
		bh = bh->b_this_page;
		block++;
	} while (bh != head);

	do {
		get_bh(bh);
		if (buffer_dirty(bh)) {
			lock_buffer(bh);
			if (buffer_dirty(bh)) {
				if (!buffer_mapped(bh))
					buffer_error();
				if (!buffer_uptodate(bh))
					buffer_error();
				set_buffer_async_io(bh);
			} else {
				unlock_buffer(bh);
			}
		}
		bh = bh->b_this_page;
	} while (bh != head);

	/*
	 * The page may come unlocked any time after the *first* submit_bh()
	 * call.  Be careful with its buffers.
	 */
	do {
		struct buffer_head *next = bh->b_this_page;
		if (buffer_async(bh)) {
			mark_buffer_clean(bh);
			submit_bh(WRITE, bh);
			nr_underway++;
		}
		put_bh(bh);
		bh = next;
	} while (bh != head);

	err = 0;
done:
	if (nr_underway == 0) {
		/*
		 * The page was marked dirty, but the buffers were
		 * clean.  Someone wrote them back by hand with
		 * ll_rw_block/submit_bh.  A rare case.
		 */
		int uptodate = 1;
		do {
			if (!buffer_uptodate(bh)) {
				uptodate = 0;
				break;
			}
			bh = bh->b_this_page;
		} while (bh != head);
		if (uptodate)
			SetPageUptodate(page);
		unlock_page(page);
	}
	return err;
recover:
	/*
	 * ENOSPC, or some other error.  We may already have added some
	 * blocks to the file, so we need to write these out to avoid
	 * exposing stale data.
	 */
	ClearPageUptodate(page);
	bh = head;
	/* Recovery: lock and submit the mapped buffers */
	do {
		if (buffer_mapped(bh)) {
			lock_buffer(bh);
			set_buffer_async_io(bh);
		} else {
			/*
			 * The buffer may have been set dirty during
			 * attachment to a dirty page.
			 */
			mark_buffer_clean(bh);
		}
		bh = bh->b_this_page;
	} while (bh != head);
	do {
		struct buffer_head *next = bh->b_this_page;
		if (buffer_mapped(bh)) {
			mark_buffer_uptodate(bh, 1);
			mark_buffer_clean(bh);
			submit_bh(WRITE, bh);
			nr_underway++;
		}
		bh = next;
	} while (bh != head);
	goto done;
}

static int __block_prepare_write(struct inode *inode, struct page *page,
		unsigned from, unsigned to, get_block_t *get_block)
{
	unsigned block_start, block_end;
	unsigned long block;
	int err = 0;
	unsigned blocksize, bbits;
	struct buffer_head *bh, *head, *wait[2], **wait_bh=wait;
	char *kaddr = kmap(page);

	BUG_ON(!PageLocked(page));
	BUG_ON(from > PAGE_CACHE_SIZE);
	BUG_ON(to > PAGE_CACHE_SIZE);
	BUG_ON(from > to);

	blocksize = 1 << inode->i_blkbits;
	if (!page_has_buffers(page))
		create_empty_buffers(page, blocksize, 0);
	head = page_buffers(page);

	bbits = inode->i_blkbits;
	block = page->index << (PAGE_CACHE_SHIFT - bbits);

	for(bh = head, block_start = 0; bh != head || !block_start;
	    block++, block_start=block_end, bh = bh->b_this_page) {
		block_end = block_start + blocksize;
		if (block_end <= from || block_start >= to) {
			if (PageUptodate(page))
				mark_buffer_uptodate(bh, 1);
			continue;
		}
		clear_bit(BH_New, &bh->b_state);
		if (!buffer_mapped(bh)) {
			err = get_block(inode, block, bh, 1);
			if (err)
				goto out;
			if (buffer_new(bh)) {
				clear_bit(BH_New, &bh->b_state);
				unmap_underlying_metadata(bh);
				if (PageUptodate(page)) {
					if (!buffer_mapped(bh))
						buffer_error();
					mark_buffer_uptodate(bh, 1);
					continue;
				}
				if (block_end > to)
					memset(kaddr+to, 0, block_end-to);
				if (block_start < from)
					memset(kaddr+block_start,
						0, from-block_start);
				if (block_end > to || block_start < from)
					flush_dcache_page(page);
				continue;
			}
		}
		if (PageUptodate(page)) {
			mark_buffer_uptodate(bh, 1);
			continue; 
		}
		if (!buffer_uptodate(bh) &&
		     (block_start < from || block_end > to)) {
			ll_rw_block(READ, 1, &bh);
			*wait_bh++=bh;
		}
	}
	/*
	 * If we issued read requests - let them complete.
	 */
	while(wait_bh > wait) {
		wait_on_buffer(*--wait_bh);
		if (!buffer_uptodate(*wait_bh))
			return -EIO;
	}
	return 0;
out:
	/*
	 * Zero out any newly allocated blocks to avoid exposing stale
	 * data.  If BH_New is set, we know that the block was newly
	 * allocated in the above loop.
	 */
	bh = head;
	block_start = 0;
	do {
		block_end = block_start+blocksize;
		if (block_end <= from)
			goto next_bh;
		if (block_start >= to)
			break;
		if (buffer_new(bh)) {
			clear_bit(BH_New, &bh->b_state);
			if (buffer_uptodate(bh))
				buffer_error();
			memset(kaddr+block_start, 0, bh->b_size);
			mark_buffer_uptodate(bh, 1);
			mark_buffer_dirty(bh);
		}
next_bh:
		block_start = block_end;
		bh = bh->b_this_page;
	} while (bh != head);
	return err;
}

static int __block_commit_write(struct inode *inode, struct page *page,
		unsigned from, unsigned to)
{
	unsigned block_start, block_end;
	int partial = 0;
	unsigned blocksize;
	struct buffer_head *bh, *head;

	blocksize = 1 << inode->i_blkbits;

	for(bh = head = page_buffers(page), block_start = 0;
	    bh != head || !block_start;
	    block_start=block_end, bh = bh->b_this_page) {
		block_end = block_start + blocksize;
		if (block_end <= from || block_start >= to) {
			if (!buffer_uptodate(bh))
				partial = 1;
		} else {
			mark_buffer_uptodate(bh, 1);
			if (!atomic_set_buffer_dirty(bh)) {
				__mark_dirty(bh);
				buffer_insert_inode_data_queue(bh, inode);
			}
		}
	}

	/*
	 * If this is a partial write which happened to make all buffers
	 * uptodate then we can optimize away a bogus readpage() for
	 * the next read(). Here we 'discover' whether the page went
	 * uptodate as a result of this (potentially partial) write.
	 */
	if (!partial)
		SetPageUptodate(page);
	return 0;
}

/*
 * Generic "read page" function for block devices that have the normal
 * get_block functionality. This is most of the block device filesystems.
 * Reads the page asynchronously --- the unlock_buffer() and
 * mark_buffer_uptodate() functions propagate buffer state into the
 * page struct once IO has completed.
 */
int block_read_full_page(struct page *page, get_block_t *get_block)
{
	struct inode *inode = page->mapping->host;
	unsigned long iblock, lblock;
	struct buffer_head *bh, *head, *arr[MAX_BUF_PER_PAGE];
	unsigned int blocksize, blocks;
	int nr, i;

	if (!PageLocked(page))
		PAGE_BUG(page);
	if (PageUptodate(page))
		buffer_error();
	blocksize = 1 << inode->i_blkbits;
	if (!page_has_buffers(page))
		create_empty_buffers(page, blocksize, 0);
	head = page_buffers(page);

	blocks = PAGE_CACHE_SIZE >> inode->i_blkbits;
	iblock = page->index << (PAGE_CACHE_SHIFT - inode->i_blkbits);
	lblock = (inode->i_size+blocksize-1) >> inode->i_blkbits;
	bh = head;
	nr = 0;
	i = 0;

	do {
		if (buffer_uptodate(bh))
			continue;

		if (!buffer_mapped(bh)) {
			if (iblock < lblock) {
				if (get_block(inode, iblock, bh, 0))
					SetPageError(page);
			}
			if (!buffer_mapped(bh)) {
				memset(kmap(page) + i*blocksize, 0, blocksize);
				flush_dcache_page(page);
				kunmap(page);
				mark_buffer_uptodate(bh, 1);
				continue;
			}
			/*
			 * get_block() might have updated the buffer
			 * synchronously
			 */
			if (buffer_uptodate(bh))
				continue;
		}
		arr[nr++] = bh;
	} while (i++, iblock++, (bh = bh->b_this_page) != head);

	if (!nr) {
		/*
		 * All buffers are uptodate - we can set the page uptodate
		 * as well. But not if get_block() returned an error.
		 */
		if (!PageError(page))
			SetPageUptodate(page);
		unlock_page(page);
		return 0;
	}

	/* Stage two: lock the buffers */
	for (i = 0; i < nr; i++) {
		struct buffer_head * bh = arr[i];
		lock_buffer(bh);
		if (buffer_uptodate(bh))
			buffer_error();
		if (buffer_dirty(bh))
			buffer_error();
		set_buffer_async_io(bh);
	}

	/*
	 * Stage 3: start the IO.  Check for uptodateness
	 * inside the buffer lock in case another process reading
	 * the underlying blockdev brought it uptodate (the sct fix).
	 */
	for (i = 0; i < nr; i++) {
		struct buffer_head * bh = arr[i];
		if (buffer_uptodate(bh))
			end_buffer_io_async(bh, 1);
		else
			submit_bh(READ, bh);
	}
	return 0;
}

/* utility function for filesystems that need to do work on expanding
 * truncates.  Uses prepare/commit_write to allow the filesystem to
 * deal with the hole.  
 */
int generic_cont_expand(struct inode *inode, loff_t size)
{
	struct address_space *mapping = inode->i_mapping;
	struct page *page;
	unsigned long index, offset, limit;
	int err;

	err = -EFBIG;
        limit = current->rlim[RLIMIT_FSIZE].rlim_cur;
	if (limit != RLIM_INFINITY && size > (loff_t)limit) {
		send_sig(SIGXFSZ, current, 0);
		goto out;
	}
	if (size > inode->i_sb->s_maxbytes)
		goto out;

	offset = (size & (PAGE_CACHE_SIZE-1)); /* Within page */

	/* ugh.  in prepare/commit_write, if from==to==start of block, we 
	** skip the prepare.  make sure we never send an offset for the start
	** of a block
	*/
	if ((offset & (inode->i_sb->s_blocksize - 1)) == 0) {
		offset++;
	}
	index = size >> PAGE_CACHE_SHIFT;
	err = -ENOMEM;
	page = grab_cache_page(mapping, index);
	if (!page)
		goto out;
	err = mapping->a_ops->prepare_write(NULL, page, offset, offset);
	if (!err) {
		err = mapping->a_ops->commit_write(NULL, page, offset, offset);
	}
	unlock_page(page);
	page_cache_release(page);
	if (err > 0)
		err = 0;
out:
	return err;
}

/*
 * For moronic filesystems that do not allow holes in file.
 * We may have to extend the file.
 */

int cont_prepare_write(struct page *page, unsigned offset,
		unsigned to, get_block_t *get_block, unsigned long *bytes)
{
	struct address_space *mapping = page->mapping;
	struct inode *inode = mapping->host;
	struct page *new_page;
	unsigned long pgpos;
	long status;
	unsigned zerofrom;
	unsigned blocksize = 1 << inode->i_blkbits;
	char *kaddr;

	while(page->index > (pgpos = *bytes>>PAGE_CACHE_SHIFT)) {
		status = -ENOMEM;
		new_page = grab_cache_page(mapping, pgpos);
		if (!new_page)
			goto out;
		/* we might sleep */
		if (*bytes>>PAGE_CACHE_SHIFT != pgpos) {
			unlock_page(new_page);
			page_cache_release(new_page);
			continue;
		}
		zerofrom = *bytes & ~PAGE_CACHE_MASK;
		if (zerofrom & (blocksize-1)) {
			*bytes |= (blocksize-1);
			(*bytes)++;
		}
		status = __block_prepare_write(inode, new_page, zerofrom,
						PAGE_CACHE_SIZE, get_block);
		if (status)
			goto out_unmap;
		kaddr = page_address(new_page);
		memset(kaddr+zerofrom, 0, PAGE_CACHE_SIZE-zerofrom);
		flush_dcache_page(new_page);
		__block_commit_write(inode, new_page,
				zerofrom, PAGE_CACHE_SIZE);
		kunmap(new_page);
		unlock_page(new_page);
		page_cache_release(new_page);
	}

	if (page->index < pgpos) {
		/* completely inside the area */
		zerofrom = offset;
	} else {
		/* page covers the boundary, find the boundary offset */
		zerofrom = *bytes & ~PAGE_CACHE_MASK;

		/* if we will expand the thing last block will be filled */
		if (to > zerofrom && (zerofrom & (blocksize-1))) {
			*bytes |= (blocksize-1);
			(*bytes)++;
		}

		/* starting below the boundary? Nothing to zero out */
		if (offset <= zerofrom)
			zerofrom = offset;
	}
	status = __block_prepare_write(inode, page, zerofrom, to, get_block);
	if (status)
		goto out1;
	kaddr = page_address(page);
	if (zerofrom < offset) {
		memset(kaddr+zerofrom, 0, offset-zerofrom);
		flush_dcache_page(page);
		__block_commit_write(inode, page, zerofrom, offset);
	}
	return 0;
out1:
	ClearPageUptodate(page);
	kunmap(page);
	return status;

out_unmap:
	ClearPageUptodate(new_page);
	kunmap(new_page);
	unlock_page(new_page);
	page_cache_release(new_page);
out:
	return status;
}

int block_prepare_write(struct page *page, unsigned from, unsigned to,
			get_block_t *get_block)
{
	struct inode *inode = page->mapping->host;
	int err = __block_prepare_write(inode, page, from, to, get_block);
	if (err) {
		ClearPageUptodate(page);
		kunmap(page);
	}
	return err;
}

int block_commit_write(struct page *page, unsigned from, unsigned to)
{
	struct inode *inode = page->mapping->host;
	__block_commit_write(inode,page,from,to);
	kunmap(page);
	return 0;
}

int generic_commit_write(struct file *file, struct page *page,
		unsigned from, unsigned to)
{
	struct inode *inode = page->mapping->host;
	loff_t pos = ((loff_t)page->index << PAGE_CACHE_SHIFT) + to;
	__block_commit_write(inode,page,from,to);
	kunmap(page);
	if (pos > inode->i_size) {
		inode->i_size = pos;
		mark_inode_dirty(inode);
	}
	return 0;
}

int block_truncate_page(struct address_space *mapping,
			loff_t from, get_block_t *get_block)
{
	unsigned long index = from >> PAGE_CACHE_SHIFT;
	unsigned offset = from & (PAGE_CACHE_SIZE-1);
	unsigned blocksize, iblock, length, pos;
	struct inode *inode = mapping->host;
	struct page *page;
	struct buffer_head *bh;
	int err;

	blocksize = 1 << inode->i_blkbits;
	length = offset & (blocksize - 1);

	/* Block boundary? Nothing to do */
	if (!length)
		return 0;

	length = blocksize - length;
	iblock = index << (PAGE_CACHE_SHIFT - inode->i_blkbits);
	
	page = grab_cache_page(mapping, index);
	err = -ENOMEM;
	if (!page)
		goto out;

	if (!page_has_buffers(page))
		create_empty_buffers(page, blocksize, 0);

	/* Find the buffer that contains "offset" */
	bh = page_buffers(page);
	pos = blocksize;
	while (offset >= pos) {
		bh = bh->b_this_page;
		iblock++;
		pos += blocksize;
	}

	err = 0;
	if (!buffer_mapped(bh)) {
		/* Hole? Nothing to do */
		if (buffer_uptodate(bh))
			goto unlock;
		get_block(inode, iblock, bh, 0);
		/* Still unmapped? Nothing to do */
		if (!buffer_mapped(bh))
			goto unlock;
	}

	/* Ok, it's mapped. Make sure it's up-to-date */
	if (PageUptodate(page))
		mark_buffer_uptodate(bh, 1);

	if (!buffer_uptodate(bh)) {
		err = -EIO;
		ll_rw_block(READ, 1, &bh);
		wait_on_buffer(bh);
		/* Uhhuh. Read error. Complain and punt. */
		if (!buffer_uptodate(bh))
			goto unlock;
	}

	memset(kmap(page) + offset, 0, length);
	flush_dcache_page(page);
	kunmap(page);

	mark_buffer_dirty(bh);
	err = 0;

unlock:
	unlock_page(page);
	page_cache_release(page);
out:
	return err;
}

/*
 * The generic ->writepage function for buffer-backed address_spaces
 */
int block_write_full_page(struct page *page, get_block_t *get_block)
{
	struct inode * const inode = page->mapping->host;
	const unsigned long end_index = inode->i_size >> PAGE_CACHE_SHIFT;
	unsigned offset;
	char *kaddr;

	/* Is the page fully inside i_size? */
	if (page->index < end_index)
		return __block_write_full_page(inode, page, get_block);

	/* Is the page fully outside i_size? (truncate in progress) */
	offset = inode->i_size & (PAGE_CACHE_SIZE-1);
	if (page->index >= end_index+1 || !offset) {
		unlock_page(page);
		return -EIO;
	}

	/* The page straddles i_size */
	kaddr = kmap(page);
	memset(kaddr + offset, 0, PAGE_CACHE_SIZE - offset);
	flush_dcache_page(page);
	kunmap(page);
	return __block_write_full_page(inode, page, get_block);
}

/*
 * Commence writeout of all the buffers against a page.  The
 * page must be locked.   Returns zero on success or a negative
 * errno.
 */
int writeout_one_page(struct page *page)
{
	struct buffer_head * const head = page_buffers(page);
	struct buffer_head *arr[MAX_BUF_PER_PAGE];
	struct buffer_head *bh;
	int nr = 0;
	BUG_ON(!PageLocked(page));
	bh = head;
	do {
		if (!buffer_locked(bh) && buffer_dirty(bh) &&
				buffer_mapped(bh) && buffer_uptodate(bh))
			arr[nr++] = bh;
	} while ((bh = bh->b_this_page) != head);
	if (nr)
		ll_rw_block(WRITE, nr, arr);	
	return 0;
}
EXPORT_SYMBOL(writeout_one_page);

/*
 * Wait for completion of I/O of all buffers against a page.  The page
 * must be locked.  Returns zero on success or a negative errno.
 */
int waitfor_one_page(struct page *page)
{
	int error = 0;
	struct buffer_head *bh, *head = page_buffers(page);

	bh = head;
	do {
		wait_on_buffer(bh);
		if (buffer_req(bh) && !buffer_uptodate(bh))
			error = -EIO;
	} while ((bh = bh->b_this_page) != head);
	return error;
}
EXPORT_SYMBOL(waitfor_one_page);

sector_t generic_block_bmap(struct address_space *mapping, sector_t block,
			    get_block_t *get_block)
{
	struct buffer_head tmp;
	struct inode *inode = mapping->host;
	tmp.b_state = 0;
	tmp.b_blocknr = 0;
	get_block(inode, block, &tmp, 0);
	return tmp.b_blocknr;
}

int generic_direct_IO(int rw, struct inode *inode,
			struct kiobuf *iobuf, unsigned long blocknr,
			int blocksize, get_block_t *get_block)
{
	int i, nr_blocks, retval;
	sector_t *blocks = iobuf->blocks;

	nr_blocks = iobuf->length / blocksize;
	/* build the blocklist */
	for (i = 0; i < nr_blocks; i++, blocknr++) {
		struct buffer_head bh;

		bh.b_state = 0;
		bh.b_size = blocksize;

		retval = get_block(inode, blocknr, &bh, rw & 1);
		if (retval)
			goto out;

		if (rw == READ) {
			if (buffer_new(&bh))
				BUG();
			if (!buffer_mapped(&bh)) {
				/* there was an hole in the filesystem */
				blocks[i] = -1UL;
				continue;
			}
		} else {
			if (buffer_new(&bh))
				unmap_underlying_metadata(&bh);
			if (!buffer_mapped(&bh))
				BUG();
		}
		blocks[i] = bh.b_blocknr;
	}

	/* This does not understand multi-device filesystems currently */
	retval = brw_kiovec(rw, 1, &iobuf,
			inode->i_sb->s_bdev, blocks, blocksize);

 out:
	return retval;
}

/*
 * Start I/O on a physical range of kernel memory, defined by a vector
 * of kiobuf structs (much like a user-space iovec list).
 *
 * The kiobuf must already be locked for IO.  IO is submitted
 * asynchronously: you need to check page->locked and page->uptodate.
 *
 * It is up to the caller to make sure that there are enough blocks
 * passed in to completely map the iobufs to disk.
 */
int brw_kiovec(int rw, int nr, struct kiobuf *iovec[],
	       struct block_device *bdev, sector_t b[], int size)
{
	int		transferred;
	int		i;
	int		err;
	struct kiobuf *	iobuf;

	if (!nr)
		return 0;
	
	/* 
	 * First, do some alignment and validity checks 
	 */
	for (i = 0; i < nr; i++) {
		iobuf = iovec[i];
		if ((iobuf->offset & (size-1)) || (iobuf->length & (size-1)))
			return -EINVAL;
		if (!iobuf->nr_pages)
			panic("brw_kiovec: iobuf not initialised");
	}

	/* 
	 * OK to walk down the iovec doing page IO on each page we find. 
	 */
	for (i = 0; i < nr; i++) {
		iobuf = iovec[i];
		iobuf->errno = 0;

		ll_rw_kio(rw, iobuf, bdev, b[i] * (size >> 9));
	}

	/*
	 * now they are all submitted, wait for completion
	 */
	transferred = 0;
	err = 0;
	for (i = 0; i < nr; i++) {
		iobuf = iovec[i];
		kiobuf_wait_for_io(iobuf);
		if (iobuf->errno && !err)
			err = iobuf->errno;
		if (!err)
			transferred += iobuf->length;
	}

	return err ? err : transferred;
}

/*
 * Start I/O on a page.
 * This function expects the page to be locked and may return
 * before I/O is complete. You then have to check page->locked
 * and page->uptodate.
 *
 * FIXME: we need a swapper_inode->get_block function to remove
 *        some of the bmap kludges and interface ugliness here.
 */
int brw_page(int rw, struct page *page,
		struct block_device *bdev, sector_t b[], int size)
{
	struct buffer_head *head, *bh;

	BUG_ON(!PageLocked(page));

	if (!page_has_buffers(page))
		create_empty_buffers(page, size, 0);
	head = bh = page_buffers(page);

	/* Stage 1: lock all the buffers */
	do {
		lock_buffer(bh);
		bh->b_blocknr = *(b++);
		bh->b_bdev = bdev;
		set_bit(BH_Mapped, &bh->b_state);
		if (rw == WRITE)	/* To support submit_bh debug tests */
			mark_buffer_uptodate(bh, 1);
		set_buffer_async_io(bh);
		bh = bh->b_this_page;
	} while (bh != head);

	/* Stage 2: start the IO */
	do {
		struct buffer_head *next = bh->b_this_page;
		submit_bh(rw, bh);
		bh = next;
	} while (bh != head);
	return 0;
}

int block_symlink(struct inode *inode, const char *symname, int len)
{
	struct address_space *mapping = inode->i_mapping;
	struct page *page = grab_cache_page(mapping, 0);
	int err = -ENOMEM;
	char *kaddr;

	if (!page)
		goto fail;
	err = mapping->a_ops->prepare_write(NULL, page, 0, len-1);
	if (err)
		goto fail_map;
	kaddr = page_address(page);
	memcpy(kaddr, symname, len-1);
	mapping->a_ops->commit_write(NULL, page, 0, len-1);
	/*
	 * Notice that we are _not_ going to block here - end of page is
	 * unmapped, so this will only try to map the rest of page, see
	 * that it is unmapped (typically even will not look into inode -
	 * ->i_size will be enough for everything) and zero it out.
	 * OTOH it's obviously correct and should make the page up-to-date.
	 */
	err = mapping->a_ops->readpage(NULL, page);
	wait_on_page(page);
	page_cache_release(page);
	if (err < 0)
		goto fail;
	mark_inode_dirty(inode);
	return 0;
fail_map:
	unlock_page(page);
	page_cache_release(page);
fail:
	return err;
}

/*
 * Sanity checks for try_to_free_buffers.
 */
static void check_ttfb_buffer(struct page *page, struct buffer_head *bh)
{
	if (!buffer_uptodate(bh)) {
		if (PageUptodate(page) && page->mapping
			&& buffer_mapped(bh)	/* discard_buffer */
			&& S_ISBLK(page->mapping->host->i_mode))
		{
			buffer_error();
		}
	}
}

/*
 * try_to_free_buffers() checks if all the buffers on this particular page
 * are unused, and releases them if so.
 *
 * Exclusion against try_to_free_buffers may be obtained by either
 * locking the page or by holding its inode's i_bufferlist_lock.
 *
 * If the page is dirty but all the buffers are clean then we need to
 * be sure to mark the page clean as well.  This is because the page
 * may be against a block device, and a later reattachment of buffers
 * to a dirty page will set *all* buffers dirty.  Which would corrupt
 * filesystem data on the same device.
 *
 * The same applies to regular filesystem pages: if all the buffers are
 * clean then we set the page clean and proceed.  To do that, we require
 * total exclusion from __set_page_dirty_buffers().  That is obtained with
 * i_bufferlist_lock.
 *
 * Nobody should be calling try_to_free_buffers against a page which is
 * eligible for set_page_dirty() treatment anyway - the page is clearly
 * not freeable.  So we could just test page_count(page) here and complain
 * then scram if it's wrong.
 *
 * If any buffer is not uptodate then the entire page is set not uptodate,
 * as the partial uptodateness information is about to be lost.
 *
 * try_to_free_buffers() is non-blocking.
 */
static inline int buffer_busy(struct buffer_head *bh)
{
	return atomic_read(&bh->b_count) |
		(bh->b_state & ((1 << BH_Dirty) | (1 << BH_Lock)));
}

static /*inline*/ int drop_buffers(struct page *page)
{
	struct buffer_head *head = page_buffers(page);
	struct buffer_head *bh;
	int was_uptodate = 1;

	bh = head;
	do {
		check_ttfb_buffer(page, bh);
		if (buffer_busy(bh))
			goto failed;
		if (!buffer_uptodate(bh))
			was_uptodate = 0;
		bh = bh->b_this_page;
	} while (bh != head);

	if (!was_uptodate && PageUptodate(page))
		buffer_error();

	do {
		struct buffer_head *next = bh->b_this_page;

		__remove_inode_queue(bh);
		free_buffer_head(bh);
		bh = next;
	} while (bh != head);
	__clear_page_buffers(page);
	return 1;
failed:
	return 0;
}

int try_to_free_buffers(struct page *page)
{
	struct inode *inode;
	int ret = 0;

	BUG_ON(!PageLocked(page));

	if (page->mapping == NULL)	/* swapped-in anon page */
		return drop_buffers(page);

	inode = page->mapping->host;
	spin_lock(&inode->i_bufferlist_lock);
	ret = drop_buffers(page);
	if (ret)
		ClearPageDirty(page);
	spin_unlock(&inode->i_bufferlist_lock);
	return ret;
}
EXPORT_SYMBOL(try_to_free_buffers);

/* ================== Debugging =================== */

void show_buffers(void)
{
	printk("Buffer memory:   %6dkB\n",
			atomic_read(&buffermem_pages) << (PAGE_SHIFT-10));
}

int block_sync_page(struct page *page)
{
	run_task_queue(&tq_disk);
	return 0;
}

/*
 * There are no bdflush tunables left.  But distributions are
 * still running obsolete flush daemons, so we terminate them here.
 */
asmlinkage long sys_bdflush(int func, long data)
{
	if (!capable(CAP_SYS_ADMIN))
		return -EPERM;
	if (func == 1)
		do_exit(0);
	return 0;
}

void wakeup_bdflush(void)
{
 	pdflush_flush(0);
}

/*
 * Buffer-head allocation
 */
static kmem_cache_t *bh_cachep;
static mempool_t *bh_mempool;

struct buffer_head *alloc_buffer_head(int async)
{
	return mempool_alloc(bh_mempool, GFP_NOFS);
}
EXPORT_SYMBOL(alloc_buffer_head);

void free_buffer_head(struct buffer_head *bh)
{
	if (bh->b_inode)
		BUG();
	mempool_free(bh, bh_mempool);
}
EXPORT_SYMBOL(free_buffer_head);

static void init_buffer_head(void *data, kmem_cache_t *cachep, unsigned long flags)
{
	if ((flags & (SLAB_CTOR_VERIFY|SLAB_CTOR_CONSTRUCTOR)) ==
			    SLAB_CTOR_CONSTRUCTOR) {
		struct buffer_head * bh = (struct buffer_head *)data;

		memset(bh, 0, sizeof(*bh));
		bh->b_blocknr = -1;
		init_waitqueue_head(&bh->b_wait);
	}
}

static void *bh_mempool_alloc(int gfp_mask, void *pool_data)
{
	return kmem_cache_alloc(bh_cachep, gfp_mask);
}

static void bh_mempool_free(void *element, void *pool_data)
{
	return kmem_cache_free(bh_cachep, element);
}

#define NR_RESERVED (10*MAX_BUF_PER_PAGE)
#define MAX_UNUSED_BUFFERS NR_RESERVED+20

void __init buffer_init(void)
{
	bh_cachep = kmem_cache_create("buffer_head",
			sizeof(struct buffer_head), 0,
			SLAB_HWCACHE_ALIGN, init_buffer_head, NULL);
	bh_mempool = mempool_create(MAX_UNUSED_BUFFERS, bh_mempool_alloc,
				bh_mempool_free, NULL);
}
