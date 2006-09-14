/*
 * mm/page-writeback.c.
 *
 * Copyright (C) 2002, Linus Torvalds.
 *
 * Contains functions related to writing back dirty pages at the
 * address_space level.
 *
 * 10Apr2002	akpm@zip.com.au
 *		Initial version
 */

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/spinlock.h>
#include <linux/fs.h>
#include <linux/mm.h>
#include <linux/slab.h>
#include <linux/pagemap.h>
#include <linux/writeback.h>
#include <linux/init.h>
#include <linux/sysrq.h>

/*
 * Memory thresholds, in percentages
 * FIXME: expose these via /proc or whatever.
 */

/*
 * Start background writeback (via pdflush) at this level
 */
static int dirty_background_ratio = 30;

/*
 * The generator of dirty data starts async writeback at this level
 */
static int dirty_async_ratio = 45;

/*
 * The generator of dirty data performs sync writeout at this level
 */
static int dirty_sync_ratio = 60;

/*
 * balance_dirty_pages() must be called by processes which are
 * generating dirty data.  It looks at the number of dirty pages
 * in the machine and either:
 *
 * - Starts background writeback or
 * - Causes the caller to perform async writeback or
 * - Causes the caller to perform synchronous writeback, then
 *   tells a pdflush thread to perform more writeback or
 * - Does nothing at all.
 *
 * balance_dirty_pages() can sleep.
 */
void balance_dirty_pages(struct address_space *mapping)
{
	const int tot = nr_free_pagecache_pages();
	struct page_state ps;
	int background_thresh;
	int async_thresh;
	int sync_thresh;
	int wake_pdflush = 0;
	unsigned long dirty_and_locked;

	get_page_state(&ps);
	dirty_and_locked = ps.nr_dirty + ps.nr_locked;

	background_thresh = (dirty_background_ratio * tot) / 100;
	async_thresh = (dirty_async_ratio * tot) / 100;
	sync_thresh = (dirty_sync_ratio * tot) / 100;

	if (dirty_and_locked > sync_thresh) {
		int nr_to_write = dirty_and_locked - async_thresh;

		writeback_unlocked_inodes(&nr_to_write, WB_SYNC_LAST, NULL);
		wake_pdflush = 1;
	} else if (dirty_and_locked > async_thresh) {
		int nr_to_write = dirty_and_locked - async_thresh;

		writeback_unlocked_inodes(&nr_to_write, WB_SYNC_NONE, NULL);
	} else if (dirty_and_locked > background_thresh) {
		wake_pdflush = 1;
	}

	if (wake_pdflush && !IS_FLUSHING(mapping->host)) {
		/*
		 * There is no flush thread against this device. Start one now.
		 */
		get_page_state(&ps);
		if (ps.nr_dirty > 0) {
			pdflush_flush(ps.nr_dirty);
			yield();
		}
	}
}

/*
 * Front-end to balance_dirty_pages - just to make sure it's not called
 * too often.
 */
void balance_dirty_pages_ratelimited(struct address_space *mapping)
{
	static struct rate_limit_struct {
		int count;
	} ____cacheline_aligned ratelimits[NR_CPUS];
	int cpu;

	preempt_disable();
	cpu = smp_processor_id();
	if (ratelimits[cpu].count++ >= 32) {
		ratelimits[cpu].count = 0;
		preempt_enable();
		balance_dirty_pages(mapping);
		return;
	}
	preempt_enable();
}

/*
 * Here are some applications of the pdflush thread pool
 */

/*
 * Start heavy writeback of everything.  This is the analogue of the old
 * wakeup_bdflush().  Returns zero if a thread was successfully launched.
 *
 * Is passed in the number of pages to write.
 *
 * We yield, to allow page allocators to perform their I/O against large files.
 */

static void pdflush_bdflush(unsigned long arg)
{
	int nr_pages = arg;

	CHECK_EMERGENCY_SYNC

	while (nr_pages) {
		int nr_to_write = WRITEOUT_PAGES;

		if (nr_to_write > nr_pages)
			nr_to_write = nr_pages;
		nr_pages -= nr_to_write;
		writeback_unlocked_inodes(&nr_to_write, WB_SYNC_NONE, NULL);
		yield();
	}
	run_task_queue(&tq_disk);
}

int pdflush_flush(unsigned long nr_pages)
{
	return pdflush_operation(pdflush_bdflush, nr_pages);
}

/*
 * The interval between `kupdate'-style writebacks.
 *
 * Traditional kupdate writes back data which is 30-35 seconds old.
 * This one does that, but it also writes back just 1/6th of the dirty
 * data.  This is to avoid great I/O storms.
 *
 * We chunk the writes up and yield, to permit any throttled page-allocators
 * to perform their I/O against a large file.
 */
static int wb_writeback_jifs = 5 * HZ;

/*
 * Periodic writeback of "old" data.
 *
 * Define "old": the first time one of an inode's pages is dirtied, we mark the
 * dirtying-time in the inode's address_space.  So this periodic writeback code
 * just walks the superblock inode list, writing back any inodes which are
 * older than a specific point in time.
 *
 * Spot the bug: at jiffies wraparound, the attempt to set the inode's dirtying
 * time won't work, because zero means not-dirty.  That's OK. The data will get
 * written out later by the VM (at least).
 *
 * We also limit the number of pages which are written out, to avoid writing
 * huge amounts of data against a single file, which would cause memory
 * allocators to block for too long.
 */
static void wb_kupdate(unsigned long arg)
{
	unsigned long oldest_jif = jiffies - 30*HZ;
	struct page_state ps;
	int total_to_write;
	int nr_to_write;

	sync_supers();

	get_page_state(&ps);

	total_to_write = ps.nr_dirty / 6;
	if (total_to_write < 16384) {
		total_to_write = 16384;
		if (total_to_write > ps.nr_dirty)
			total_to_write = ps.nr_dirty;
	}
	while (total_to_write > 0) {
		nr_to_write = total_to_write;
		if (nr_to_write > WRITEOUT_PAGES)
			nr_to_write = WRITEOUT_PAGES;
		total_to_write -= nr_to_write;
		writeback_unlocked_inodes(&nr_to_write,
				WB_SYNC_NONE, &oldest_jif);
		yield();
	}
	run_task_queue(&tq_disk);
}

/*
 * The writeback timer, for kupdate-style functionality
 */
static struct timer_list wb_timer;

static void wb_timer_fn(unsigned long unused)
{
	mod_timer(&wb_timer, jiffies + wb_writeback_jifs);
	pdflush_operation(wb_kupdate, 0);
}

static int __init wb_timer_init(void)
{
	init_timer(&wb_timer);
	wb_timer.expires = jiffies + wb_writeback_jifs;
	wb_timer.data = 0;
	wb_timer.function = wb_timer_fn;
	add_timer(&wb_timer);
	return 0;
}
module_init(wb_timer_init);

/*
 * FIXME: PG_launder gets cleared by accident.
 */
static int writeback_mapping(struct page *page, int *nr_to_write)
{
	struct inode *inode = page->mapping->host;

	SetPageDirty(page);

	/*
	 * We don't own this inode, so we don't want the address_space
	 * vanishing while writeback is walking the list
	 */
	inode = igrab(inode);
	unlock_page(page);

	if (inode) {
		writeback_single_inode(inode, 0, nr_to_write);

		/*
		 * This iput() will internally call ext2_discard_prealloc(),
		 * which is rather bogus.  But there is no other way of
		 * dropping our ref to the inode.  However, there's no harm
		 * in dropping the prealloc, because there probably isn't any.
		 * Just a waste of cycles.
		 */
		iput(inode);
	}
	return 0;
}

/*
 * A library function, which implements the vm_writeback a_op.  It's fairly
 * lame at this time.  The idea is: the VM wants to liberate this page,
 * so we pass the page to the address_space and give the fs the opportunity
 * to write out lots of pages around this one.  It allows extent-based
 * filesytems to do intelligent things.  It lets delayed-allocate filesystems
 * perform better file layout.  It lets the address_space opportunistically
 * write back disk-contiguous pages which are in other zones.
 */
int generic_vm_writeback(struct page *page, int *nr_to_write)
{
	return writeback_mapping(page, nr_to_write);
}
EXPORT_SYMBOL(generic_vm_writeback);

/**
 * generic_writeback_mapping - walk the list of dirty pages of the given
 * address space and writepage() all of them.
 * 
 * @mapping: address space structure to write
 * @nr_to_write: subtract the number of written pages from *@nr_to_write
 *
 * This is a library function, which implements the writeback_mapping()
 * address_space_operation for filesystems which are using multipage BIO
 * writeback.
 *
 * We need to be careful to avoid deadlocks here.  mpage_bio_writepage() does
 * not immediately start I/O against each page.  It waits until the bio is
 * full, or until mpage_bio_flush() is called.  So generic_writeback_mapping()
 * is locking multiple pages without necessarily starting I/O against them.
 *
 * AB/BA deadlocks are avoided via locking implemented in the filesystem.
 * Only one process ever has multiple locked pages against any mapping.
 *
 * FIXME: doing the locking in the fs is a bit grotty, but it allows us to
 * not have to put a new semaphore in struct inode.  The fs could
 * pass its bio_write_state up here, I guess.
 *
 * Pages can be moved from clean_pages or locked_pages onto dirty_pages
 * at any time - it's not possible to lock against that.  So pages which
 * have already been added to a BIO may magically reappear on the dirty_pages
 * list.  And generic_writeback_mapping() will again try to lock those pages.
 * But I/O has not yet been started agains the page.  Thus deadlock.
 *
 * To avoid this, the entire contents of the dirty_pages list are moved
 * onto io_pages up-front.  We then walk io_pages, locking the
 * pages and submitting them for I/O, moving them to locked_pages.
 *
 * This has the added benefit of preventing a livelock which would otherwise
 * occur if pages are being dirtied faster than we can write them out.
 *
 * Thus generic_writeback_mapping() only makes the guarantee that all pages
 * which were dirty at the time it was called will have I/O started against
 * them.  And it's not possible to make a stronger guarantee than that.
 */
int generic_writeback_mapping(struct address_space *mapping, int *nr_to_write)
{
	int ret = 0;
	int done = 0;
	int err;
	int (*writepage)(struct page *) = mapping->a_ops->writepage;

	write_lock(&mapping->page_lock);

	list_splice(&mapping->dirty_pages, &mapping->io_pages);
	INIT_LIST_HEAD(&mapping->dirty_pages);
	mapping->dirtied_when = 0;

        while (!list_empty(&mapping->io_pages) && !done) {
		struct page *page = list_entry(mapping->io_pages.prev,
					struct page, list);
		list_del(&page->list);
		list_add(&page->list, &mapping->locked_pages);
		if (!PageDirty(page))
			continue;

		page_cache_get(page);
		write_unlock(&mapping->page_lock);

		lock_page(page);

		if (TestClearPageDirty(page)) {
			err = writepage(page);
			if (!ret)
				ret = err;
			if (nr_to_write) {
				--(*nr_to_write);
				if (*nr_to_write <= 0)
					done = 1;
			}
		} else
			unlock_page(page);

		page_cache_release(page);
		write_lock(&mapping->page_lock);
	}
	if (!list_empty(&mapping->io_pages)) {
		/*
		 * Put the rest back, in the correct order.
		 */
		list_splice(&mapping->io_pages, mapping->dirty_pages.prev);
		INIT_LIST_HEAD(&mapping->io_pages);
	}
	write_unlock(&mapping->page_lock);
	return ret;
}
EXPORT_SYMBOL(generic_writeback_mapping);

/*
 * Add a page to the dirty page list.
 *
 * It is a sad fact of life that this function is called from several places
 * deeply under spinlocking.  It may not sleep.
 *
 * If the page has buffers, the uptodate buffers are set dirty, to preserve
 * dirty-state coherency between the page and the buffers.  It the page does
 * not have buffers then when they are later attached they will all be set
 * dirty.
 *
 * The buffers are dirtied before the page is dirtied.  There's a small race
 * window in which a writepage caller may see the page cleanness but not the
 * buffer dirtiness.  That's fine.  If this code were to set the page dirty
 * before the buffers, a concurrent writepage caller could clear the page dirty
 * bit, see a bunch of clean buffers and we'd end up with dirty buffers/clean
 * page on the dirty page list.
 *
 * There is also a small window where the page is dirty, and not on dirty_pages.
 * Also a possibility that by the time the page is added to dirty_pages, it has
 * been set clean.  The page lists are somewhat approximate in this regard.
 * It's better to have clean pages accidentally attached to dirty_pages than to
 * leave dirty pages attached to clean_pages.
 *
 * We use i_bufferlist_lock to lock against try_to_free_buffers while using the
 * page's buffer list.  Also use this to protect against clean buffers being
 * added to the page after it was set dirty.
 *
 * FIXME: may need to call ->reservepage here as well.  That's rather up to the
 * address_space though.
 */
int __set_page_dirty_buffers(struct page *page)
{
	int ret = 0;
	struct address_space *mapping = page->mapping;
	struct inode *inode;

	if (mapping == NULL) {
		SetPageDirty(page);
		goto out;
	}

	inode = mapping->host;

	spin_lock(&inode->i_bufferlist_lock);

	if (page_has_buffers(page)) {
		struct buffer_head *head = page_buffers(page);
		struct buffer_head *bh = head;

		do {
			if (buffer_uptodate(bh))
				set_bit(BH_Dirty, &bh->b_state);
			bh = bh->b_this_page;
		} while (bh != head);
	}

	if (!TestSetPageDirty(page)) {
		write_lock(&mapping->page_lock);
		list_del(&page->list);
		list_add(&page->list, &mapping->dirty_pages);
		write_unlock(&mapping->page_lock);
		__mark_inode_dirty(mapping->host, I_DIRTY_PAGES);
	}
	
	spin_unlock(&inode->i_bufferlist_lock);
out:
	return ret;
}
EXPORT_SYMBOL(__set_page_dirty_buffers);

/*
 * For address_spaces which do not use buffers.  Just set the page's dirty bit
 * and move it to the dirty_pages list.  Also perform space reservation if
 * required.
 *
 * __set_page_dirty_nobuffers() may return -ENOSPC.  But if it does, the page
 * is still safe, as long as it actually manages to find some blocks at
 * writeback time.
 *
 * This is also used when a single buffer is being dirtied: we want to set the
 * page dirty in that case, but not all the buffers.  This is a "bottom-up"
 * dirtying, whereas __set_page_dirty_buffers() is a "top-down" dirtying.
 */
int __set_page_dirty_nobuffers(struct page *page)
{
	int ret = 0;

	if (!TestSetPageDirty(page)) {
		struct address_space *mapping = page->mapping;

		if (mapping) {
			write_lock(&mapping->page_lock);
			list_del(&page->list);
			list_add(&page->list, &mapping->dirty_pages);
			write_unlock(&mapping->page_lock);
			__mark_inode_dirty(mapping->host, I_DIRTY_PAGES);
		}
	}
	return ret;
}
EXPORT_SYMBOL(__set_page_dirty_nobuffers);
