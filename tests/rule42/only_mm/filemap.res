/*
 *	linux/mm/filemap.c
 *
 * Copyright (C) 1994-1999  Linus Torvalds
 */

/*
 * This file handles the generic file mmap semantics used by
 * most "normal" filesystems (but you don't /have/ to use this:
 * the NFS filesystem used to do this differently, for example)
 */
#include <linux/module.h>
#include <linux/slab.h>
#include <linux/compiler.h>
#include <linux/fs.h>
#include <linux/mm.h>
#include <linux/mman.h>
#include <linux/pagemap.h>
#include <linux/file.h>
#include <linux/iobuf.h>
#include <linux/hash.h>
#include <linux/writeback.h>

#include <asm/uaccess.h>
#include <asm/mman.h>

/*
 * Shared mappings implemented 30.11.1994. It's not fully working yet,
 * though.
 *
 * Shared mappings now work. 15.8.1995  Bruno.
 *
 * finished 'unifying' the page and buffer cache and SMP-threaded the
 * page-cache, 21.05.1999, Ingo Molnar <mingo@redhat.com>
 *
 * SMP-threaded pagemap-LRU 1999, Andrea Arcangeli <andrea@suse.de>
 */


/*
 * Lock ordering:
 *
 *  pagemap_lru_lock
 *  ->i_shared_lock		(vmtruncate)
 *    ->i_bufferlist_lock	(__free_pte->__set_page_dirty_buffers)
 *      ->unused_list_lock	(try_to_free_buffers)
 *        ->mapping->page_lock
 *      ->inode_lock		(__mark_inode_dirty)
 *        ->sb_lock		(fs/fs-writeback.c)
 */
spinlock_t pagemap_lru_lock __cacheline_aligned_in_smp = SPIN_LOCK_UNLOCKED;

/*
 * Remove a page from the page cache and free it. Caller has to make
 * sure the page is locked and that nobody else uses it - or that usage
 * is safe.  The caller must hold a write_lock on the mapping's page_lock.
 */
void __remove_inode_page(struct page *page)
{
	struct address_space *mapping = page->mapping;

	if (unlikely(PageDirty(page)))
		BUG();

	radix_tree_delete(&page->mapping->page_tree, page->index);
	list_del(&page->list);
	page->mapping = NULL;

	mapping->nrpages--;
	dec_page_state(nr_pagecache);
}

void remove_inode_page(struct page *page)
{
	struct address_space *mapping = page->mapping;

	if (unlikely(!PageLocked(page)))
		PAGE_BUG(page);

	write_lock(&mapping->page_lock);
	__remove_inode_page(page);
	write_unlock(&mapping->page_lock);
}

static inline int sync_page(struct page *page)
{
	struct address_space *mapping = page->mapping;

	if (mapping && mapping->a_ops && mapping->a_ops->sync_page)
		return mapping->a_ops->sync_page(page);
	return 0;
}

/**
 * invalidate_inode_pages - Invalidate all the unlocked pages of one inode
 * @inode: the inode which pages we want to invalidate
 *
 * This function only removes the unlocked pages, if you want to
 * remove all the pages of one inode, you must call truncate_inode_pages.
 */

void invalidate_inode_pages(struct inode * inode)
{
	struct list_head *head, *curr;
	struct page * page;
	struct address_space *mapping = inode->i_mapping;

	head = &mapping->clean_pages;

	spin_lock(&pagemap_lru_lock);
	write_lock(&mapping->page_lock);
	curr = head->next;

	while (curr != head) {
		page = list_entry(curr, struct page, list);
		curr = curr->next;

		/* We cannot invalidate something in dirty.. */
		if (PageDirty(page))
			continue;

		/* ..or locked */
		if (TestSetPageLocked(page))
			continue;

		if (PagePrivate(page) && !try_to_release_page(page, 0))
			goto unlock;

		if (page_count(page) != 1)
			goto unlock;

		__lru_cache_del(page);
		__remove_inode_page(page);
		unlock_page(page);
		page_cache_release(page);
		continue;
unlock:
		unlock_page(page);
		continue;
	}

	write_unlock(&mapping->page_lock);
	spin_unlock(&pagemap_lru_lock);
}

static int do_flushpage(struct page *page, unsigned long offset)
{
	int (*flushpage) (struct page *, unsigned long);
	flushpage = page->mapping->a_ops->flushpage;
	if (flushpage)
		return (*flushpage)(page, offset);
	return block_flushpage(page, offset);
}

static inline void truncate_partial_page(struct page *page, unsigned partial)
{
	memclear_highpage_flush(page, partial, PAGE_CACHE_SIZE-partial);
	if (PagePrivate(page))
		do_flushpage(page, partial);
}

/*
 * AKPM: the PagePrivate test here seems a bit bogus.  It bypasses the
 * mapping's ->flushpage, which may still want to be called.
 */
static void truncate_complete_page(struct page *page)
{
	/* Leave it on the LRU if it gets converted into anonymous buffers */
	if (!PagePrivate(page) || do_flushpage(page, 0))
		lru_cache_del(page);
	ClearPageDirty(page);
	ClearPageUptodate(page);
	remove_inode_page(page);
	page_cache_release(page);
}

/*
 * Writeback walks the page list in ->prev order, which is low-to-high file
 * offsets in the common case where he file was written linearly. So truncate
 * walks the page list in the opposite (->next) direction, to avoid getting
 * into lockstep with writeback's cursor.  To prune as many pages as possible
 * before the truncate cursor collides with the writeback cursor.
 */
static int truncate_list_pages(struct address_space *mapping,
	struct list_head *head, unsigned long start, unsigned *partial)
{
	struct list_head *curr;
	struct page * page;
	int unlocked = 0;

 restart:
	curr = head->next;
	while (curr != head) {
		unsigned long offset;

		page = list_entry(curr, struct page, list);
		offset = page->index;

		/* Is one of the pages to truncate? */
		if ((offset >= start) || (*partial && (offset + 1) == start)) {
			int failed;

			page_cache_get(page);
			failed = TestSetPageLocked(page);

			list_del(head);
			if (!failed)
				/* Restart after this page */
				list_add(head, curr);
			else
				/* Restart on this page */
				list_add_tail(head, curr);

			write_unlock(&mapping->page_lock);
			unlocked = 1;

 			if (!failed) {
				if (*partial && (offset + 1) == start) {
					truncate_partial_page(page, *partial);
					*partial = 0;
				} else 
					truncate_complete_page(page);

				unlock_page(page);
			} else
 				wait_on_page(page);

			page_cache_release(page);

			if (need_resched()) {
				__set_current_state(TASK_RUNNING);
				schedule();
			}

			write_lock(&mapping->page_lock);
			goto restart;
		}
		curr = curr->next;
	}
	return unlocked;
}

/**
 * truncate_inode_pages - truncate *all* the pages from an offset
 * @mapping: mapping to truncate
 * @lstart: offset from with to truncate
 *
 * Truncate the page cache at a set offset, removing the pages
 * that are beyond that offset (and zeroing out partial pages).
 * If any page is locked we wait for it to become unlocked.
 */
void truncate_inode_pages(struct address_space * mapping, loff_t lstart) 
{
	unsigned long start = (lstart + PAGE_CACHE_SIZE - 1) >> PAGE_CACHE_SHIFT;
	unsigned partial = lstart & (PAGE_CACHE_SIZE - 1);
	int unlocked;

	write_lock(&mapping->page_lock);
	do {
		unlocked |= truncate_list_pages(mapping,
				&mapping->io_pages, start, &partial);
		unlocked |= truncate_list_pages(mapping,
				&mapping->dirty_pages, start, &partial);
		unlocked = truncate_list_pages(mapping,
				&mapping->clean_pages, start, &partial);
		unlocked |= truncate_list_pages(mapping,
				&mapping->locked_pages, start, &partial);
	} while (unlocked);
	/* Traversed all three lists without dropping the lock */
	write_unlock(&mapping->page_lock);
}

static inline int invalidate_this_page2(struct address_space * mapping,
					struct page * page,
					struct list_head * curr,
					struct list_head * head)
{
	int unlocked = 1;

	/*
	 * The page is locked and we hold the mapping lock as well
	 * so both page_count(page) and page_buffers stays constant here.
	 * AKPM: fixme: No global lock any more.  Is this still OK?
	 */
	if (page_count(page) == 1 + !!page_has_buffers(page)) {
		/* Restart after this page */
		list_del(head);
		list_add_tail(head, curr);

		page_cache_get(page);
		write_unlock(&mapping->page_lock);
		truncate_complete_page(page);
	} else {
		if (page_has_buffers(page)) {
			/* Restart after this page */
			list_del(head);
			list_add_tail(head, curr);

			page_cache_get(page);
			write_unlock(&mapping->page_lock);
			block_flushpage(page, 0);
		} else
			unlocked = 0;

		ClearPageDirty(page);
		ClearPageUptodate(page);
	}

	return unlocked;
}

static int invalidate_list_pages2(struct address_space * mapping,
				  struct list_head * head)
{
	struct list_head *curr;
	struct page * page;
	int unlocked = 0;

 restart:
	curr = head->prev;
	while (curr != head) {
		page = list_entry(curr, struct page, list);

		if (!TestSetPageLocked(page)) {
			int __unlocked;

			__unlocked = invalidate_this_page2(mapping, page, curr, head);
			unlock_page(page);
			unlocked |= __unlocked;
			if (!__unlocked) {
				curr = curr->prev;
				continue;
			}
		} else {
			/* Restart on this page */
			list_del(head);
			list_add(head, curr);

			page_cache_get(page);
			write_unlock(&mapping->page_lock);
			unlocked = 1;
			wait_on_page(page);
		}

		page_cache_release(page);
		if (need_resched()) {
			__set_current_state(TASK_RUNNING);
			schedule();
		}

		write_lock(&mapping->page_lock);
		goto restart;
	}
	return unlocked;
}

/**
 * invalidate_inode_pages2 - Clear all the dirty bits around if it can't
 * free the pages because they're mapped.
 * @mapping: the address_space which pages we want to invalidate
 */
void invalidate_inode_pages2(struct address_space * mapping)
{
	int unlocked;

	write_lock(&mapping->page_lock);
	do {
		unlocked = invalidate_list_pages2(mapping,
				&mapping->clean_pages);
		unlocked |= invalidate_list_pages2(mapping,
				&mapping->dirty_pages);
		unlocked |= invalidate_list_pages2(mapping,
				&mapping->io_pages);
		unlocked |= invalidate_list_pages2(mapping,
				&mapping->locked_pages);
	} while (unlocked);
	write_unlock(&mapping->page_lock);
}

static int do_buffer_fdatasync(struct address_space *mapping,
		struct list_head *head, unsigned long start,
		unsigned long end, int (*fn)(struct page *))
{
	struct list_head *curr;
	struct page *page;
	int retval = 0;

	write_lock(&mapping->page_lock);
	curr = head->next;
	while (curr != head) {
		page = list_entry(curr, struct page, list);
		curr = curr->next;
		if (!page_has_buffers(page))
			continue;
		if (page->index >= end)
			continue;
		if (page->index < start)
			continue;

		page_cache_get(page);
		write_unlock(&mapping->page_lock);
		lock_page(page);

		/* The buffers could have been free'd while we waited for the page lock */
		if (page_has_buffers(page))
			retval |= fn(page);

		unlock_page(page);
		write_lock(&mapping->page_lock);
		curr = page->list.next;
		page_cache_release(page);
	}
	write_unlock(&mapping->page_lock);

	return retval;
}

/*
 * Two-stage data sync: first start the IO, then go back and
 * collect the information..
 */
int generic_buffer_fdatasync(struct inode *inode, unsigned long start_idx, unsigned long end_idx)
{
	struct address_space *mapping = inode->i_mapping;
	int retval;

	/* writeout dirty buffers on pages from both clean and dirty lists */
	retval = do_buffer_fdatasync(mapping, &mapping->dirty_pages,
			start_idx, end_idx, writeout_one_page);
	retval = do_buffer_fdatasync(mapping, &mapping->io_pages,
			start_idx, end_idx, writeout_one_page);
	retval |= do_buffer_fdatasync(mapping, &mapping->clean_pages,
			start_idx, end_idx, writeout_one_page);
	retval |= do_buffer_fdatasync(mapping, &mapping->locked_pages,
			start_idx, end_idx, writeout_one_page);

	/* now wait for locked buffers on pages from both clean and dirty lists */
	retval |= do_buffer_fdatasync(mapping, &mapping->dirty_pages,
			start_idx, end_idx, waitfor_one_page);
	retval |= do_buffer_fdatasync(mapping, &mapping->io_pages,
			start_idx, end_idx, waitfor_one_page);
	retval |= do_buffer_fdatasync(mapping, &mapping->clean_pages,
			start_idx, end_idx, waitfor_one_page);
	retval |= do_buffer_fdatasync(mapping, &mapping->locked_pages,
			start_idx, end_idx, waitfor_one_page);

	return retval;
}

/*
 * In-memory filesystems have to fail their
 * writepage function - and this has to be
 * worked around in the VM layer..
 *
 * We
 *  - mark the page dirty again (but do NOT
 *    add it back to the inode dirty list, as
 *    that would livelock in fdatasync)
 *  - activate the page so that the page stealer
 *    doesn't try to write it out over and over
 *    again.
 */
int fail_writepage(struct page *page)
{
	/* Only activate on memory-pressure, not fsync.. */
	if (PageLaunder(page)) {
		activate_page(page);
		SetPageReferenced(page);
	}

	/* Set the page dirty again, unlock */
	SetPageDirty(page);
	unlock_page(page);
	return 0;
}

EXPORT_SYMBOL(fail_writepage);

/**
 *  filemap_fdatasync - walk the list of dirty pages of the given address space
 *                      and writepage() all of them.
 *
 *  @mapping: address space structure to write
 *
 */
int filemap_fdatasync(struct address_space *mapping)
{
	if (mapping->a_ops->writeback_mapping)
		return mapping->a_ops->writeback_mapping(mapping, NULL);
	return generic_writeback_mapping(mapping, NULL);
}

/**
 *      filemap_fdatawait - walk the list of locked pages of the given address space
 *     	and wait for all of them.
 * 
 *      @mapping: address space structure to wait for
 *
 */
int filemap_fdatawait(struct address_space * mapping)
{
	int ret = 0;

	write_lock(&mapping->page_lock);

        while (!list_empty(&mapping->locked_pages)) {
		struct page *page = list_entry(mapping->locked_pages.next, struct page, list);

		list_del(&page->list);
		list_add(&page->list, &mapping->clean_pages);

		if (!PageLocked(page))
			continue;

		page_cache_get(page);
		write_unlock(&mapping->page_lock);

		___wait_on_page(page);
		if (PageError(page))
			ret = -EIO;

		page_cache_release(page);
		write_lock(&mapping->page_lock);
	}
	write_unlock(&mapping->page_lock);
	return ret;
}

/*
 * This adds a page to the page cache, starting out as locked,
 * owned by us, but unreferenced, not uptodate and with no errors.
 * The caller must hold a write_lock on the mapping->page_lock.
 */
static int __add_to_page_cache(struct page *page,
		struct address_space *mapping, unsigned long offset)
{
	page_cache_get(page);
	if (radix_tree_insert(&mapping->page_tree, offset, page) < 0)
		goto nomem;
	page->flags &= ~(1 << PG_uptodate | 1 << PG_error |
			1 << PG_referenced | 1 << PG_arch_1 | 1 << PG_checked);
	SetPageLocked(page);
	ClearPageDirty(page);
	___add_to_page_cache(page, mapping, offset);
	return 0;
 nomem:
	page_cache_release(page);
	return -ENOMEM;
}

int add_to_page_cache(struct page *page,
		struct address_space *mapping, unsigned long offset)
{
	write_lock(&mapping->page_lock);
	if (__add_to_page_cache(page, mapping, offset) < 0)
		goto nomem;
	write_unlock(&mapping->page_lock);
	lru_cache_add(page);
	return 0;
nomem:
	write_unlock(&mapping->page_lock);
	return -ENOMEM;
}

int add_to_page_cache_unique(struct page *page,
		struct address_space *mapping, unsigned long offset)
{
	struct page *alias;
	int error = -EEXIST;

	write_lock(&mapping->page_lock);
	if (!(alias = radix_tree_lookup(&mapping->page_tree, offset)))
		error = __add_to_page_cache(page, mapping, offset);
	write_unlock(&mapping->page_lock);

	if (!error)
		lru_cache_add(page);
	return error;
}

/*
 * This adds the requested page to the page cache if it isn't already there,
 * and schedules an I/O to read in its contents from disk.
 */
static int FASTCALL(page_cache_read(struct file * file, unsigned long offset));
static int page_cache_read(struct file * file, unsigned long offset)
{
	struct address_space *mapping = file->f_dentry->d_inode->i_mapping;
	struct page *page; 
	int error;

	read_lock(&mapping->page_lock);
	page = radix_tree_lookup(&mapping->page_tree, offset);
	read_unlock(&mapping->page_lock);
	if (page)
		return 0;

	page = page_cache_alloc(mapping);
	if (!page)
		return -ENOMEM;

	error = add_to_page_cache_unique(page, mapping, offset);
	if (!error) {
		error = mapping->a_ops->readpage(file, page);
		page_cache_release(page);
		return error;
	}

	/*
	 * We arrive here in the unlikely event that someone 
	 * raced with us and added our page to the cache first
	 * or we are out of memory for radix-tree nodes.
	 */
	page_cache_release(page);
	return error == -EEXIST ? 0 : error;
}

/*
 * In order to wait for pages to become available there must be
 * waitqueues associated with pages. By using a hash table of
 * waitqueues where the bucket discipline is to maintain all
 * waiters on the same queue and wake all when any of the pages
 * become available, and for the woken contexts to check to be
 * sure the appropriate page became available, this saves space
 * at a cost of "thundering herd" phenomena during rare hash
 * collisions.
 */
static inline wait_queue_head_t *page_waitqueue(struct page *page)
{
	const zone_t *zone = page_zone(page);

	return &zone->wait_table[hash_ptr(page, zone->wait_table_bits)];
}

/* 
 * Wait for a page to get unlocked.
 *
 * This must be called with the caller "holding" the page,
 * ie with increased "page->count" so that the page won't
 * go away during the wait..
 */
void ___wait_on_page(struct page *page)
{
	wait_queue_head_t *waitqueue = page_waitqueue(page);
	struct task_struct *tsk = current;
	DECLARE_WAITQUEUE(wait, tsk);

	add_wait_queue(waitqueue, &wait);
	do {
		set_task_state(tsk, TASK_UNINTERRUPTIBLE);
		if (!PageLocked(page))
			break;
		sync_page(page);
		schedule();
	} while (PageLocked(page));
	__set_task_state(tsk, TASK_RUNNING);
	remove_wait_queue(waitqueue, &wait);
}

/*
 * Unlock the page and wake up sleepers in ___wait_on_page.
 *
 * The first mb is necessary to safely close the critical section opened by the
 * TryLockPage(), the second mb is necessary to enforce ordering between
 * the clear_bit and the read of the waitqueue (to avoid SMP races with a
 * parallel wait_on_page).
 */
void unlock_page(struct page *page)
{
	wait_queue_head_t *waitqueue = page_waitqueue(page);
	clear_bit(PG_launder, &(page)->flags);
	smp_mb__before_clear_bit();
	if (!TestClearPageLocked(page))
		BUG();
	smp_mb__after_clear_bit(); 
	if (waitqueue_active(waitqueue))
		wake_up_all(waitqueue);
}

/*
 * Get a lock on the page, assuming we need to sleep
 * to get it..
 */
static void __lock_page(struct page *page)
{
	wait_queue_head_t *waitqueue = page_waitqueue(page);
	struct task_struct *tsk = current;
	DECLARE_WAITQUEUE(wait, tsk);

	add_wait_queue_exclusive(waitqueue, &wait);
	for (;;) {
		set_task_state(tsk, TASK_UNINTERRUPTIBLE);
		if (PageLocked(page)) {
			sync_page(page);
			schedule();
		}
		if (!TestSetPageLocked(page))
			break;
	}
	__set_task_state(tsk, TASK_RUNNING);
	remove_wait_queue(waitqueue, &wait);
}

void wake_up_page(struct page *page)
{
	wake_up(page_waitqueue(page));
}
EXPORT_SYMBOL(wake_up_page);

/*
 * Get an exclusive lock on the page, optimistically
 * assuming it's not locked..
 */
void lock_page(struct page *page)
{
	if (TestSetPageLocked(page))
		__lock_page(page);
}

/*
 * a rather lightweight function, finding and getting a reference to a
 * hashed page atomically.
 */
struct page * find_get_page(struct address_space *mapping, unsigned long offset)
{
	struct page *page;

	/*
	 * We scan the hash list read-only. Addition to and removal from
	 * the hash-list needs a held write-lock.
	 */
	read_lock(&mapping->page_lock);
	page = radix_tree_lookup(&mapping->page_tree, offset);
	if (page)
		page_cache_get(page);
	read_unlock(&mapping->page_lock);
	return page;
}

/*
 * Same as above, but trylock it instead of incrementing the count.
 */
struct page *find_trylock_page(struct address_space *mapping, unsigned long offset)
{
	struct page *page;

	read_lock(&mapping->page_lock);
	page = radix_tree_lookup(&mapping->page_tree, offset);
	if (page && TestSetPageLocked(page))
		page = NULL;
	read_unlock(&mapping->page_lock);
	return page;
}

/*
 * Must be called with the mapping lock held for writing.
 * Will return with it held for writing, but it may be dropped
 * while locking the page.
 */
static struct page *__find_lock_page(struct address_space *mapping,
					unsigned long offset)
{
	struct page *page;

	/*
	 * We scan the hash list read-only. Addition to and removal from
	 * the hash-list needs a held write-lock.
	 */
repeat:
	page = radix_tree_lookup(&mapping->page_tree, offset);
	if (page) {
		page_cache_get(page);
		if (TestSetPageLocked(page)) {
			write_unlock(&mapping->page_lock);
			lock_page(page);
			write_lock(&mapping->page_lock);

			/* Has the page been truncated while we slept? */
			if (page->mapping != mapping || page->index != offset) {
				unlock_page(page);
				page_cache_release(page);
				goto repeat;
			}
		}
	}
	return page;
}

/**
 * find_lock_page - locate, pin and lock a pagecache page
 *
 * @mapping - the address_space to search
 * @offset - the page index
 *
 * Locates the desired pagecache page, locks it, increments its reference
 * count and returns its address.
 *
 * Returns zero if the page was not present. find_lock_page() may sleep.
 */

/*
 * The write_lock is unfortunate, but __find_lock_page() requires that on
 * behalf of find_or_create_page().  We could just clone __find_lock_page() -
 * one for find_lock_page(), one for find_or_create_page()...
 */
struct page *find_lock_page(struct address_space *mapping,
				unsigned long offset)
{
	struct page *page;

	write_lock(&mapping->page_lock);
	page = __find_lock_page(mapping, offset);
	write_unlock(&mapping->page_lock);
	return page;
}

/**
 * find_or_create_page - locate or add a pagecache page
 *
 * @mapping - the page's address_space
 * @index - the page's index into the mapping
 * @gfp_mask - page allocation mode
 *
 * Locates a page in the pagecache.  If the page is not present, a new page
 * is allocated using @gfp_mask and is added to the pagecache and to the VM's
 * LRU list.  The returned page is locked and has its reference count
 * incremented.
 *
 * find_or_create_page() may sleep, even if @gfp_flags specifies an atomic
 * allocation!
 *
 * find_or_create_page() returns the desired page's address, or zero on
 * memory exhaustion.
 */
struct page *find_or_create_page(struct address_space *mapping,
		unsigned long index, unsigned int gfp_mask)
{
	struct page *page;

	page = find_lock_page(mapping, index);
	if (!page) {
		struct page *newpage = alloc_page(gfp_mask);
		if (newpage) {
			write_lock(&mapping->page_lock);
			page = __find_lock_page(mapping, index);
			if (likely(!page)) {
				page = newpage;
				if (__add_to_page_cache(page, mapping, index)) {
					write_unlock(&mapping->page_lock);
					page_cache_release(page);
					page = NULL;
					goto out;
				}
				newpage = NULL;
			}
			write_unlock(&mapping->page_lock);
			if (newpage == NULL)
				lru_cache_add(page);
			else 
				page_cache_release(newpage);
		}
	}
out:
	return page;
}

/*
 * Returns locked page at given index in given cache, creating it if needed.
 */
struct page *grab_cache_page(struct address_space *mapping, unsigned long index)
{
	return find_or_create_page(mapping, index, mapping->gfp_mask);
}


/*
 * Same as grab_cache_page, but do not wait if the page is unavailable.
 * This is intended for speculative data generators, where the data can
 * be regenerated if the page couldn't be grabbed.  This routine should
 * be safe to call while holding the lock for another page.
 */
struct page *grab_cache_page_nowait(struct address_space *mapping, unsigned long index)
{
	struct page *page;

	page = find_get_page(mapping, index);

	if ( page ) {
		if ( !TestSetPageLocked(page) ) {
			/* Page found and locked */
			/* This test is overly paranoid, but what the heck... */
			if ( unlikely(page->mapping != mapping || page->index != index) ) {
				/* Someone reallocated this page under us. */
				unlock_page(page);
				page_cache_release(page);
				return NULL;
			} else {
				return page;
			}
		} else {
			/* Page locked by someone else */
			page_cache_release(page);
			return NULL;
		}
	}

	page = page_cache_alloc(mapping);
	if (unlikely(!page))
		return NULL;	/* Failed to allocate a page */

	if (unlikely(add_to_page_cache_unique(page, mapping, index))) {
		/*
		 * Someone else grabbed the page already, or
		 * failed to allocate a radix-tree node
		 */
		page_cache_release(page);
		return NULL;
	}

	return page;
}

/*
 * Mark a page as having seen activity.
 *
 * If it was already so marked, move it
 * to the active queue and drop the referenced
 * bit. Otherwise, just mark it for future
 * action..
 */
void mark_page_accessed(struct page *page)
{
	if (!PageActive(page) && PageReferenced(page)) {
		activate_page(page);
		ClearPageReferenced(page);
		return;
	}

	/* Mark the page referenced, AFTER checking for previous usage.. */
	SetPageReferenced(page);
}

/*
 * This is a generic file read routine, and uses the
 * inode->i_op->readpage() function for the actual low-level
 * stuff.
 *
 * This is really ugly. But the goto's actually try to clarify some
 * of the logic when it comes to error handling etc.
 */
void do_generic_file_read(struct file * filp, loff_t *ppos, read_descriptor_t * desc, read_actor_t actor)
{
	struct address_space *mapping = filp->f_dentry->d_inode->i_mapping;
	struct inode *inode = mapping->host;
	unsigned long index, offset;
	struct page *cached_page;
	int error;

	cached_page = NULL;
	index = *ppos >> PAGE_CACHE_SHIFT;
	offset = *ppos & ~PAGE_CACHE_MASK;

	for (;;) {
		struct page *page;
		unsigned long end_index, nr, ret;

		end_index = inode->i_size >> PAGE_CACHE_SHIFT;
			
		if (index > end_index)
			break;
		nr = PAGE_CACHE_SIZE;
		if (index == end_index) {
			nr = inode->i_size & ~PAGE_CACHE_MASK;
			if (nr <= offset)
				break;
		}

		page_cache_readahead(filp, index);

		nr = nr - offset;

		/*
		 * Try to find the data in the page cache..
		 */

		write_lock(&mapping->page_lock);
		page = radix_tree_lookup(&mapping->page_tree, index);
		if (!page) {
			write_unlock(&mapping->page_lock);
			handle_ra_thrashing(filp);
			write_lock(&mapping->page_lock);
			goto no_cached_page;
		}
found_page:
		page_cache_get(page);
		write_unlock(&mapping->page_lock);

		if (!PageUptodate(page))
			goto page_not_up_to_date;
page_ok:
		/* If users can be writing to this page using arbitrary
		 * virtual addresses, take care about potential aliasing
		 * before reading the page on the kernel side.
		 */
		if (!list_empty(&mapping->i_mmap_shared))
			flush_dcache_page(page);

		/*
		 * Mark the page accessed if we read the beginning.
		 */
		if (!offset)
			mark_page_accessed(page);

		/*
		 * Ok, we have the page, and it's up-to-date, so
		 * now we can copy it to user space...
		 *
		 * The actor routine returns how many bytes were actually used..
		 * NOTE! This may not be the same as how much of a user buffer
		 * we filled up (we may be padding etc), so we can only update
		 * "pos" here (the actor routine has to update the user buffer
		 * pointers and the remaining count).
		 */
		ret = actor(desc, page, offset, nr);
		offset += ret;
		index += offset >> PAGE_CACHE_SHIFT;
		offset &= ~PAGE_CACHE_MASK;

		page_cache_release(page);
		if (ret == nr && desc->count)
			continue;
		break;

page_not_up_to_date:
		if (PageUptodate(page))
			goto page_ok;

		/* Get exclusive access to the page ... */
		lock_page(page);

		/* Did it get unhashed before we got the lock? */
		if (!page->mapping) {
			unlock_page(page);
			page_cache_release(page);
			continue;
		}

		/* Did somebody else fill it already? */
		if (PageUptodate(page)) {
			unlock_page(page);
			goto page_ok;
		}

readpage:
		/* ... and start the actual read. The read will unlock the page. */
		error = mapping->a_ops->readpage(filp, page);

		if (!error) {
			if (PageUptodate(page))
				goto page_ok;
			wait_on_page(page);
			if (PageUptodate(page))
				goto page_ok;
			error = -EIO;
		}

		/* UHHUH! A synchronous read error occurred. Report it */
		desc->error = error;
		page_cache_release(page);
		break;

no_cached_page:
		/*
		 * Ok, it wasn't cached, so we need to create a new
		 * page..
		 *
		 * We get here with the page cache lock held.
		 */
		if (!cached_page) {
			write_unlock(&mapping->page_lock);
			cached_page = page_cache_alloc(mapping);
			if (!cached_page) {
				desc->error = -ENOMEM;
				break;
			}

			/*
			 * Somebody may have added the page while we
			 * dropped the page cache lock. Check for that.
			 */
			write_lock(&mapping->page_lock);
			page = radix_tree_lookup(&mapping->page_tree, index);
			if (page)
				goto found_page;
		}

		/*
		 * Ok, add the new page to the hash-queues...
		 */
		if (__add_to_page_cache(cached_page, mapping, index) < 0) {
			write_unlock(&mapping->page_lock);
			desc->error = -ENOMEM;
			break;
		}
		page = cached_page;
		write_unlock(&mapping->page_lock);
		lru_cache_add(page);		
		cached_page = NULL;

		goto readpage;
	}

	*ppos = ((loff_t) index << PAGE_CACHE_SHIFT) + offset;
	if (cached_page)
		page_cache_release(cached_page);
	UPDATE_ATIME(inode);
}

static ssize_t generic_file_direct_IO(int rw, struct file * filp, char * buf, size_t count, loff_t offset)
{
	ssize_t retval;
	int new_iobuf, chunk_size, blocksize_mask, blocksize, blocksize_bits, iosize, progress;
	struct kiobuf * iobuf;
	struct address_space * mapping = filp->f_dentry->d_inode->i_mapping;
	struct inode * inode = mapping->host;

	new_iobuf = 0;
	iobuf = filp->f_iobuf;
	if (test_and_set_bit(0, &filp->f_iobuf_lock)) {
		/*
		 * A parallel read/write is using the preallocated iobuf
		 * so just run slow and allocate a new one.
		 */
		retval = alloc_kiovec(1, &iobuf);
		if (retval)
			goto out;
		new_iobuf = 1;
	}

	blocksize = 1 << inode->i_blkbits;
	blocksize_bits = inode->i_blkbits;
	blocksize_mask = blocksize - 1;
	chunk_size = KIO_MAX_ATOMIC_IO << 10;

	retval = -EINVAL;
	if ((offset & blocksize_mask) || (count & blocksize_mask))
		goto out_free;
	if (!mapping->a_ops->direct_IO)
		goto out_free;

	/*
	 * Flush to disk exclusively the _data_, metadata must remain
	 * completly asynchronous or performance will go to /dev/null.
	 */
	retval = filemap_fdatasync(mapping);
	if (retval == 0)
		retval = fsync_inode_data_buffers(inode);
	if (retval == 0)
		retval = filemap_fdatawait(mapping);
	if (retval < 0)
		goto out_free;

	progress = retval = 0;
	while (count > 0) {
		iosize = count;
		if (iosize > chunk_size)
			iosize = chunk_size;

		retval = map_user_kiobuf(rw, iobuf, (unsigned long) buf, iosize);
		if (retval)
			break;

		retval = mapping->a_ops->direct_IO(rw, inode, iobuf, (offset+progress) >> blocksize_bits, blocksize);

		if (rw == READ && retval > 0)
			mark_dirty_kiobuf(iobuf, retval);
		
		if (retval >= 0) {
			count -= retval;
			buf += retval;
			progress += retval;
		}

		unmap_kiobuf(iobuf);

		if (retval != iosize)
			break;
	}

	if (progress)
		retval = progress;

 out_free:
	if (!new_iobuf)
		clear_bit(0, &filp->f_iobuf_lock);
	else
		free_kiovec(1, &iobuf);
 out:	
	return retval;
}

int file_read_actor(read_descriptor_t * desc, struct page *page, unsigned long offset, unsigned long size)
{
	char *kaddr;
	unsigned long left, count = desc->count;

	if (size > count)
		size = count;

	kaddr = kmap(page);
	left = __copy_to_user(desc->buf, kaddr + offset, size);
	kunmap(page);
	
	if (left) {
		size -= left;
		desc->error = -EFAULT;
	}
	desc->count = count - size;
	desc->written += size;
	desc->buf += size;
	return size;
}

/*
 * This is the "read()" routine for all filesystems
 * that can use the page cache directly.
 */
ssize_t generic_file_read(struct file * filp, char * buf, size_t count, loff_t *ppos)
{
	ssize_t retval;

	if ((ssize_t) count < 0)
		return -EINVAL;

	if (filp->f_flags & O_DIRECT)
		goto o_direct;

	retval = -EFAULT;
	if (access_ok(VERIFY_WRITE, buf, count)) {
		retval = 0;

		if (count) {
			read_descriptor_t desc;

			desc.written = 0;
			desc.count = count;
			desc.buf = buf;
			desc.error = 0;
			do_generic_file_read(filp, ppos, &desc, file_read_actor);

			retval = desc.written;
			if (!retval)
				retval = desc.error;
		}
	}
 out:
	return retval;

 o_direct:
	{
		loff_t pos = *ppos, size;
		struct address_space *mapping = filp->f_dentry->d_inode->i_mapping;
		struct inode *inode = mapping->host;

		retval = 0;
		if (!count)
			goto out; /* skip atime */
		size = inode->i_size;
		if (pos < size) {
			if (pos + count > size)
				count = size - pos;
			retval = generic_file_direct_IO(READ, filp, buf, count, pos);
			if (retval > 0)
				*ppos = pos + retval;
		}
		UPDATE_ATIME(filp->f_dentry->d_inode);
		goto out;
	}
}

static int file_send_actor(read_descriptor_t * desc, struct page *page, unsigned long offset , unsigned long size)
{
	ssize_t written;
	unsigned long count = desc->count;
	struct file *file = (struct file *) desc->buf;

	if (size > count)
		size = count;

 	if (file->f_op->sendpage) {
 		written = file->f_op->sendpage(file, page, offset,
					       size, &file->f_pos, size<count);
	} else {
		char *kaddr;
		mm_segment_t old_fs;

		old_fs = get_fs();
		set_fs(KERNEL_DS);

		kaddr = kmap(page);
		written = file->f_op->write(file, kaddr + offset, size, &file->f_pos);
		kunmap(page);

		set_fs(old_fs);
	}
	if (written < 0) {
		desc->error = written;
		written = 0;
	}
	desc->count = count - written;
	desc->written += written;
	return written;
}

static ssize_t common_sendfile(int out_fd, int in_fd, loff_t *offset, size_t count, loff_t max)
{
	ssize_t retval;
	struct file * in_file, * out_file;
	struct inode * in_inode, * out_inode;

	/*
	 * Get input file, and verify that it is ok..
	 */
	retval = -EBADF;
	in_file = fget(in_fd);
	if (!in_file)
		goto out;
	if (!(in_file->f_mode & FMODE_READ))
		goto fput_in;
	retval = -EINVAL;
	in_inode = in_file->f_dentry->d_inode;
	if (!in_inode)
		goto fput_in;
	if (!in_inode->i_mapping->a_ops->readpage)
		goto fput_in;
	retval = locks_verify_area(FLOCK_VERIFY_READ, in_inode, in_file, in_file->f_pos, count);
	if (retval)
		goto fput_in;

	/*
	 * Get output file, and verify that it is ok..
	 */
	retval = -EBADF;
	out_file = fget(out_fd);
	if (!out_file)
		goto fput_in;
	if (!(out_file->f_mode & FMODE_WRITE))
		goto fput_out;
	retval = -EINVAL;
	if (!out_file->f_op || !out_file->f_op->write)
		goto fput_out;
	out_inode = out_file->f_dentry->d_inode;
	retval = locks_verify_area(FLOCK_VERIFY_WRITE, out_inode, out_file, out_file->f_pos, count);
	if (retval)
		goto fput_out;

	retval = 0;
	if (count) {
		read_descriptor_t desc;
		loff_t pos;

		if (!offset)
			offset = &in_file->f_pos;

		pos = *offset;
		retval = -EINVAL;
		if (unlikely(pos < 0))
			goto fput_out;
		if (unlikely(pos + count > max)) {
			retval = -EOVERFLOW;
			if (pos >= max)
				goto fput_out;
			count = max - pos;
		}

		desc.written = 0;
		desc.count = count;
		desc.buf = (char *) out_file;
		desc.error = 0;
		do_generic_file_read(in_file, offset, &desc, file_send_actor);

		retval = desc.written;
		if (!retval)
			retval = desc.error;
		pos = *offset;
		if (pos > max)
			retval = -EOVERFLOW;
	}

fput_out:
	fput(out_file);
fput_in:
	fput(in_file);
out:
	return retval;
}

asmlinkage ssize_t sys_sendfile(int out_fd, int in_fd, off_t *offset, size_t count)
{
	loff_t pos, *ppos = NULL;
	ssize_t ret;
	if (offset) {
		off_t off;
		if (unlikely(get_user(off, offset)))
			return -EFAULT;
		pos = off;
		ppos = &pos;
	}
	ret = common_sendfile(out_fd, in_fd, ppos, count, MAX_NON_LFS);
	if (offset && put_user(pos, offset))
		ret = -EFAULT;
	return ret;
}

asmlinkage ssize_t sys_sendfile64(int out_fd, int in_fd, loff_t *offset, size_t count)
{
	loff_t pos, *ppos = NULL;
	ssize_t ret;
	if (offset) {
		if (unlikely(copy_from_user(&pos, offset, sizeof(loff_t))))
			return -EFAULT;
		ppos = &pos;
	}
	ret = common_sendfile(out_fd, in_fd, ppos, count, MAX_LFS_FILESIZE);
	if (offset && put_user(pos, offset))
		ret = -EFAULT;
	return ret;
}

static ssize_t do_readahead(struct file *file, unsigned long index, unsigned long nr)
{
	struct address_space *mapping = file->f_dentry->d_inode->i_mapping;
	unsigned long max;

	if (!mapping || !mapping->a_ops || !mapping->a_ops->readpage)
		return -EINVAL;

	/* Limit it to a sane percentage of the inactive list.. */
	max = nr_inactive_pages / 2;
	if (nr > max)
		nr = max;

	do_page_cache_readahead(file, index, nr);
	return 0;
}

asmlinkage ssize_t sys_readahead(int fd, loff_t offset, size_t count)
{
	ssize_t ret;
	struct file *file;

	ret = -EBADF;
	file = fget(fd);
	if (file) {
		if (file->f_mode & FMODE_READ) {
			unsigned long start = offset >> PAGE_CACHE_SHIFT;
			unsigned long end = (offset + count - 1) >> PAGE_CACHE_SHIFT;
			unsigned long len = end - start + 1;
			ret = do_readahead(file, start, len);
		}
		fput(file);
	}
	return ret;
}

/*
 * filemap_nopage() is invoked via the vma operations vector for a
 * mapped memory region to read in file data during a page fault.
 *
 * The goto's are kind of ugly, but this streamlines the normal case of having
 * it in the page cache, and handles the special cases reasonably without
 * having a lot of duplicated code.
 */

struct page * filemap_nopage(struct vm_area_struct * area, unsigned long address, int unused)
{
	int error;
	struct file *file = area->vm_file;
	struct address_space *mapping = file->f_dentry->d_inode->i_mapping;
	struct inode *inode = mapping->host;
	struct page *page;
	unsigned long size, pgoff, endoff;

	pgoff = ((address - area->vm_start) >> PAGE_CACHE_SHIFT) + area->vm_pgoff;
	endoff = ((area->vm_end - area->vm_start) >> PAGE_CACHE_SHIFT) + area->vm_pgoff;

retry_all:
	/*
	 * An external ptracer can access pages that normally aren't
	 * accessible..
	 */
	size = (inode->i_size + PAGE_CACHE_SIZE - 1) >> PAGE_CACHE_SHIFT;
	if ((pgoff >= size) && (area->vm_mm == current->mm))
		return NULL;

	/* The "size" of the file, as far as mmap is concerned, isn't bigger than the mapping */
	if (size > endoff)
		size = endoff;

	/*
	 * The readahead code wants to be told about each and every page
	 * so it can build and shrink its windows appropriately
	 */
	if (VM_SequentialReadHint(area))
		page_cache_readahead(area->vm_file, pgoff);

	/*
	 * If the offset is outside the mapping size we're off the end
	 * of a privately mapped file, so we need to map a zero page.
	 */
	if ((pgoff < size) && !VM_RandomReadHint(area))
		page_cache_readaround(file, pgoff);

	/*
	 * Do we have something in the page cache already?
	 */
retry_find:
	page = find_get_page(mapping, pgoff);
	if (!page)
		goto no_cached_page;

	/*
	 * Ok, found a page in the page cache, now we need to check
	 * that it's up-to-date.
	 */
	if (!PageUptodate(page))
		goto page_not_uptodate;

success:
	/*
	 * Found the page and have a reference on it, need to check sharing
	 * and possibly copy it over to another page..
	 */
	mark_page_accessed(page);
	flush_page_to_ram(page);
	return page;

no_cached_page:
	/*
	 * We're only likely to ever get here if MADV_RANDOM is in
	 * effect.
	 */
	error = page_cache_read(file, pgoff);

	/*
	 * The page we want has now been added to the page cache.
	 * In the unlikely event that someone removed it in the
	 * meantime, we'll just come back here and read it again.
	 */
	if (error >= 0)
		goto retry_find;

	/*
	 * An error return from page_cache_read can result if the
	 * system is low on memory, or a problem occurs while trying
	 * to schedule I/O.
	 */
	if (error == -ENOMEM)
		return NOPAGE_OOM;
	return NULL;

page_not_uptodate:
	lock_page(page);

	/* Did it get unhashed while we waited for it? */
	if (!page->mapping) {
		unlock_page(page);
		page_cache_release(page);
		goto retry_all;
	}

	/* Did somebody else get it up-to-date? */
	if (PageUptodate(page)) {
		unlock_page(page);
		goto success;
	}

	if (!mapping->a_ops->readpage(file, page)) {
		wait_on_page(page);
		if (PageUptodate(page))
			goto success;
	}

	/*
	 * Umm, take care of errors if the page isn't up-to-date.
	 * Try to re-read it _once_. We do this synchronously,
	 * because there really aren't any performance issues here
	 * and we need to check for errors.
	 */
	lock_page(page);

	/* Somebody truncated the page on us? */
	if (!page->mapping) {
		unlock_page(page);
		page_cache_release(page);
		goto retry_all;
	}

	/* Somebody else successfully read it in? */
	if (PageUptodate(page)) {
		unlock_page(page);
		goto success;
	}
	ClearPageError(page);
	if (!mapping->a_ops->readpage(file, page)) {
		wait_on_page(page);
		if (PageUptodate(page))
			goto success;
	}

	/*
	 * Things didn't work out. Return zero to tell the
	 * mm layer so, possibly freeing the page cache page first.
	 */
	page_cache_release(page);
	return NULL;
}

static struct vm_operations_struct generic_file_vm_ops = {
	nopage:		filemap_nopage,
};

/* This is used for a general mmap of a disk file */

int generic_file_mmap(struct file * file, struct vm_area_struct * vma)
{
	struct address_space *mapping = file->f_dentry->d_inode->i_mapping;
	struct inode *inode = mapping->host;

	if ((vma->vm_flags & VM_SHARED) && (vma->vm_flags & VM_MAYWRITE)) {
		if (!mapping->a_ops->writepage)
			return -EINVAL;
	}
	if (!mapping->a_ops->readpage)
		return -ENOEXEC;
	UPDATE_ATIME(inode);
	vma->vm_ops = &generic_file_vm_ops;
	return 0;
}

static inline void setup_read_behavior(struct vm_area_struct * vma,
	int behavior)
{
	VM_ClearReadHint(vma);
	switch(behavior) {
		case MADV_SEQUENTIAL:
			vma->vm_flags |= VM_SEQ_READ;
			break;
		case MADV_RANDOM:
			vma->vm_flags |= VM_RAND_READ;
			break;
		default:
			break;
	}
	return;
}

static long madvise_fixup_start(struct vm_area_struct * vma,
	unsigned long end, int behavior)
{
	struct vm_area_struct * n;
	struct mm_struct * mm = vma->vm_mm;

	n = kmem_cache_alloc(vm_area_cachep, SLAB_KERNEL);
	if (!n)
		return -EAGAIN;
	*n = *vma;
	n->vm_end = end;
	setup_read_behavior(n, behavior);
	n->vm_raend = 0;
	if (n->vm_file)
		get_file(n->vm_file);
	if (n->vm_ops && n->vm_ops->open)
		n->vm_ops->open(n);
	vma->vm_pgoff += (end - vma->vm_start) >> PAGE_SHIFT;
	lock_vma_mappings(vma);
	spin_lock(&mm->page_table_lock);
	vma->vm_start = end;
	__insert_vm_struct(mm, n);
	spin_unlock(&mm->page_table_lock);
	unlock_vma_mappings(vma);
	return 0;
}

static long madvise_fixup_end(struct vm_area_struct * vma,
	unsigned long start, int behavior)
{
	struct vm_area_struct * n;
	struct mm_struct * mm = vma->vm_mm;

	n = kmem_cache_alloc(vm_area_cachep, SLAB_KERNEL);
	if (!n)
		return -EAGAIN;
	*n = *vma;
	n->vm_start = start;
	n->vm_pgoff += (n->vm_start - vma->vm_start) >> PAGE_SHIFT;
	setup_read_behavior(n, behavior);
	n->vm_raend = 0;
	if (n->vm_file)
		get_file(n->vm_file);
	if (n->vm_ops && n->vm_ops->open)
		n->vm_ops->open(n);
	lock_vma_mappings(vma);
	spin_lock(&mm->page_table_lock);
	vma->vm_end = start;
	__insert_vm_struct(mm, n);
	spin_unlock(&mm->page_table_lock);
	unlock_vma_mappings(vma);
	return 0;
}

static long madvise_fixup_middle(struct vm_area_struct * vma,
	unsigned long start, unsigned long end, int behavior)
{
	struct vm_area_struct * left, * right;
	struct mm_struct * mm = vma->vm_mm;

	left = kmem_cache_alloc(vm_area_cachep, SLAB_KERNEL);
	if (!left)
		return -EAGAIN;
	right = kmem_cache_alloc(vm_area_cachep, SLAB_KERNEL);
	if (!right) {
		kmem_cache_free(vm_area_cachep, left);
		return -EAGAIN;
	}
	*left = *vma;
	*right = *vma;
	left->vm_end = start;
	right->vm_start = end;
	right->vm_pgoff += (right->vm_start - left->vm_start) >> PAGE_SHIFT;
	left->vm_raend = 0;
	right->vm_raend = 0;
	if (vma->vm_file)
		atomic_add(2, &vma->vm_file->f_count);

	if (vma->vm_ops && vma->vm_ops->open) {
		vma->vm_ops->open(left);
		vma->vm_ops->open(right);
	}
	vma->vm_pgoff += (start - vma->vm_start) >> PAGE_SHIFT;
	vma->vm_raend = 0;
	lock_vma_mappings(vma);
	spin_lock(&mm->page_table_lock);
	vma->vm_start = start;
	vma->vm_end = end;
	setup_read_behavior(vma, behavior);
	__insert_vm_struct(mm, left);
	__insert_vm_struct(mm, right);
	spin_unlock(&mm->page_table_lock);
	unlock_vma_mappings(vma);
	return 0;
}

/*
 * We can potentially split a vm area into separate
 * areas, each area with its own behavior.
 */
static long madvise_behavior(struct vm_area_struct * vma,
	unsigned long start, unsigned long end, int behavior)
{
	int error = 0;

	/* This caps the number of vma's this process can own */
	if (vma->vm_mm->map_count > MAX_MAP_COUNT)
		return -ENOMEM;

	if (start == vma->vm_start) {
		if (end == vma->vm_end) {
			setup_read_behavior(vma, behavior);
			vma->vm_raend = 0;
		} else
			error = madvise_fixup_start(vma, end, behavior);
	} else {
		if (end == vma->vm_end)
			error = madvise_fixup_end(vma, start, behavior);
		else
			error = madvise_fixup_middle(vma, start, end, behavior);
	}

	return error;
}

/*
 * Schedule all required I/O operations, then run the disk queue
 * to make sure they are started.  Do not wait for completion.
 */
static long madvise_willneed(struct vm_area_struct * vma,
				unsigned long start, unsigned long end)
{
	long error = -EBADF;
	struct file * file;
	unsigned long size, rlim_rss;

	/* Doesn't work if there's no mapped file. */
	if (!vma->vm_file)
		return error;
	file = vma->vm_file;
	size = (file->f_dentry->d_inode->i_size + PAGE_CACHE_SIZE - 1) >>
							PAGE_CACHE_SHIFT;

	start = ((start - vma->vm_start) >> PAGE_SHIFT) + vma->vm_pgoff;
	if (end > vma->vm_end)
		end = vma->vm_end;
	end = ((end - vma->vm_start) >> PAGE_SHIFT) + vma->vm_pgoff;

	/* Make sure this doesn't exceed the process's max rss. */
	error = -EIO;
	rlim_rss = current->rlim ?  current->rlim[RLIMIT_RSS].rlim_cur :
				LONG_MAX; /* default: see resource.h */
	if ((vma->vm_mm->rss + (end - start)) > rlim_rss)
		return error;

	do_page_cache_readahead(file, start, end - start);
	return 0;
}

/*
 * Application no longer needs these pages.  If the pages are dirty,
 * it's OK to just throw them away.  The app will be more careful about
 * data it wants to keep.  Be sure to free swap resources too.  The
 * zap_page_range call sets things up for refill_inactive to actually free
 * these pages later if no one else has touched them in the meantime,
 * although we could add these pages to a global reuse list for
 * refill_inactive to pick up before reclaiming other pages.
 *
 * NB: This interface discards data rather than pushes it out to swap,
 * as some implementations do.  This has performance implications for
 * applications like large transactional databases which want to discard
 * pages in anonymous maps after committing to backing store the data
 * that was kept in them.  There is no reason to write this data out to
 * the swap area if the application is discarding it.
 *
 * An interface that causes the system to free clean pages and flush
 * dirty pages is already available as msync(MS_INVALIDATE).
 */
static long madvise_dontneed(struct vm_area_struct * vma,
	unsigned long start, unsigned long end)
{
	if (vma->vm_flags & VM_LOCKED)
		return -EINVAL;

	zap_page_range(vma, start, end - start);
	return 0;
}

static long madvise_vma(struct vm_area_struct * vma, unsigned long start,
	unsigned long end, int behavior)
{
	long error = -EBADF;

	switch (behavior) {
	case MADV_NORMAL:
	case MADV_SEQUENTIAL:
	case MADV_RANDOM:
		error = madvise_behavior(vma, start, end, behavior);
		break;

	case MADV_WILLNEED:
		error = madvise_willneed(vma, start, end);
		break;

	case MADV_DONTNEED:
		error = madvise_dontneed(vma, start, end);
		break;

	default:
		error = -EINVAL;
		break;
	}
		
	return error;
}

/*
 * The madvise(2) system call.
 *
 * Applications can use madvise() to advise the kernel how it should
 * handle paging I/O in this VM area.  The idea is to help the kernel
 * use appropriate read-ahead and caching techniques.  The information
 * provided is advisory only, and can be safely disregarded by the
 * kernel without affecting the correct operation of the application.
 *
 * behavior values:
 *  MADV_NORMAL - the default behavior is to read clusters.  This
 *		results in some read-ahead and read-behind.
 *  MADV_RANDOM - the system should read the minimum amount of data
 *		on any access, since it is unlikely that the appli-
 *		cation will need more than what it asks for.
 *  MADV_SEQUENTIAL - pages in the given range will probably be accessed
 *		once, so they can be aggressively read ahead, and
 *		can be freed soon after they are accessed.
 *  MADV_WILLNEED - the application is notifying the system to read
 *		some pages ahead.
 *  MADV_DONTNEED - the application is finished with the given range,
 *		so the kernel can free resources associated with it.
 *
 * return values:
 *  zero    - success
 *  -EINVAL - start + len < 0, start is not page-aligned,
 *		"behavior" is not a valid value, or application
 *		is attempting to release locked or shared pages.
 *  -ENOMEM - addresses in the specified range are not currently
 *		mapped, or are outside the AS of the process.
 *  -EIO    - an I/O error occurred while paging in data.
 *  -EBADF  - map exists, but area maps something that isn't a file.
 *  -EAGAIN - a kernel resource was temporarily unavailable.
 */
asmlinkage long sys_madvise(unsigned long start, size_t len, int behavior)
{
	unsigned long end;
	struct vm_area_struct * vma;
	int unmapped_error = 0;
	int error = -EINVAL;

	down_write(&current->mm->mmap_sem);

	if (start & ~PAGE_MASK)
		goto out;
	len = (len + ~PAGE_MASK) & PAGE_MASK;
	end = start + len;
	if (end < start)
		goto out;

	error = 0;
	if (end == start)
		goto out;

	/*
	 * If the interval [start,end) covers some unmapped address
	 * ranges, just ignore them, but return -ENOMEM at the end.
	 */
	vma = find_vma(current->mm, start);
	for (;;) {
		/* Still start < end. */
		error = -ENOMEM;
		if (!vma)
			goto out;

		/* Here start < vma->vm_end. */
		if (start < vma->vm_start) {
			unmapped_error = -ENOMEM;
			start = vma->vm_start;
		}

		/* Here vma->vm_start <= start < vma->vm_end. */
		if (end <= vma->vm_end) {
			if (start < end) {
				error = madvise_vma(vma, start, end,
							behavior);
				if (error)
					goto out;
			}
			error = unmapped_error;
			goto out;
		}

		/* Here vma->vm_start <= start < vma->vm_end < end. */
		error = madvise_vma(vma, start, vma->vm_end, behavior);
		if (error)
			goto out;
		start = vma->vm_end;
		vma = vma->vm_next;
	}

out:
	up_write(&current->mm->mmap_sem);
	return error;
}

static inline
struct page *__read_cache_page(struct address_space *mapping,
				unsigned long index,
				int (*filler)(void *,struct page*),
				void *data)
{
	struct page *page, *cached_page = NULL;
	int err;
repeat:
	page = find_get_page(mapping, index);
	if (!page) {
		if (!cached_page) {
			cached_page = page_cache_alloc(mapping);
			if (!cached_page)
				return ERR_PTR(-ENOMEM);
		}
		err = add_to_page_cache_unique(cached_page, mapping, index);
		if (err == -EEXIST)
			goto repeat;
		if (err < 0) {
			/* Presumably ENOMEM for radix tree node */
			page_cache_release(cached_page);
			return ERR_PTR(err);
		}
		page = cached_page;
		cached_page = NULL;
		err = filler(data, page);
		if (err < 0) {
			page_cache_release(page);
			page = ERR_PTR(err);
		}
	}
	if (cached_page)
		page_cache_release(cached_page);
	return page;
}

/*
 * Read into the page cache. If a page already exists,
 * and PageUptodate() is not set, try to fill the page.
 */
struct page *read_cache_page(struct address_space *mapping,
				unsigned long index,
				int (*filler)(void *,struct page*),
				void *data)
{
	struct page *page;
	int err;

retry:
	page = __read_cache_page(mapping, index, filler, data);
	if (IS_ERR(page))
		goto out;
	mark_page_accessed(page);
	if (PageUptodate(page))
		goto out;

	lock_page(page);
	if (!page->mapping) {
		unlock_page(page);
		page_cache_release(page);
		goto retry;
	}
	if (PageUptodate(page)) {
		unlock_page(page);
		goto out;
	}
	err = filler(data, page);
	if (err < 0) {
		page_cache_release(page);
		page = ERR_PTR(err);
	}
 out:
	return page;
}

static inline struct page * __grab_cache_page(struct address_space *mapping,
				unsigned long index, struct page **cached_page)
{
	int err;
	struct page *page;
repeat:
	page = find_lock_page(mapping, index);
	if (!page) {
		if (!*cached_page) {
			*cached_page = page_cache_alloc(mapping);
			if (!*cached_page)
				return NULL;
		}
		err = add_to_page_cache_unique(*cached_page, mapping, index);
		if (err == -EEXIST)
			goto repeat;
		if (err == 0) {
			page = *cached_page;
			*cached_page = NULL;
		}
	}
	return page;
}

inline void remove_suid(struct dentry *dentry)
{
	struct iattr newattrs;
	struct inode *inode = dentry->d_inode;
	unsigned int mode = inode->i_mode & (S_ISUID|S_ISGID|S_IXGRP);

	if (!(mode & S_IXGRP))
		mode &= S_ISUID;

	/* was any of the uid bits set? */
	if (mode && !capable(CAP_FSETID)) {
		newattrs.ia_valid = ATTR_KILL_SUID | ATTR_KILL_SGID;
		notify_change(dentry, &newattrs);
	}
}

/*
 * Write to a file through the page cache. 
 *
 * We currently put everything into the page cache prior to writing it.
 * This is not a problem when writing full pages. With partial pages,
 * however, we first have to read the data into the cache, then
 * dirty the page, and finally schedule it for writing. Alternatively, we
 * could write-through just the portion of data that would go into that
 * page, but that would kill performance for applications that write data
 * line by line, and it's prone to race conditions.
 *
 * Note that this routine doesn't try to keep track of dirty pages. Each
 * file system has to do this all by itself, unfortunately.
 *							okir@monad.swb.de
 */
ssize_t
generic_file_write(struct file *file,const char *buf,size_t count, loff_t *ppos)
{
	struct address_space *mapping = file->f_dentry->d_inode->i_mapping;
	struct inode	*inode = mapping->host;
	unsigned long	limit = current->rlim[RLIMIT_FSIZE].rlim_cur;
	loff_t		pos;
	struct page	*page, *cached_page;
	ssize_t		written;
	long		status = 0;
	int		err;
	unsigned	bytes;

	if ((ssize_t) count < 0)
		return -EINVAL;

	if (!access_ok(VERIFY_READ, buf, count))
		return -EFAULT;

	cached_page = NULL;

	down(&inode->i_sem);

	pos = *ppos;
	err = -EINVAL;
	if (pos < 0)
		goto out;

	err = file->f_error;
	if (err) {
		file->f_error = 0;
		goto out;
	}

	written = 0;

	/* FIXME: this is for backwards compatibility with 2.4 */
	if (!S_ISBLK(inode->i_mode) && file->f_flags & O_APPEND)
		pos = inode->i_size;

	/*
	 * Check whether we've reached the file size limit.
	 */
	err = -EFBIG;
	
	if (limit != RLIM_INFINITY) {
		if (pos >= limit) {
			send_sig(SIGXFSZ, current, 0);
			goto out;
		}
		if (pos > 0xFFFFFFFFULL || count > limit - (u32)pos) {
			/* send_sig(SIGXFSZ, current, 0); */
			count = limit - (u32)pos;
		}
	}

	/*
	 *	LFS rule 
	 */
	if ( pos + count > MAX_NON_LFS && !(file->f_flags&O_LARGEFILE)) {
		if (pos >= MAX_NON_LFS) {
			send_sig(SIGXFSZ, current, 0);
			goto out;
		}
		if (count > MAX_NON_LFS - (u32)pos) {
			/* send_sig(SIGXFSZ, current, 0); */
			count = MAX_NON_LFS - (u32)pos;
		}
	}

	/*
	 *	Are we about to exceed the fs block limit ?
	 *
	 *	If we have written data it becomes a short write
	 *	If we have exceeded without writing data we send
	 *	a signal and give them an EFBIG.
	 *
	 *	Linus frestrict idea will clean these up nicely..
	 */
	 
	if (!S_ISBLK(inode->i_mode)) {
		if (pos >= inode->i_sb->s_maxbytes)
		{
			if (count || pos > inode->i_sb->s_maxbytes) {
				send_sig(SIGXFSZ, current, 0);
				err = -EFBIG;
				goto out;
			}
			/* zero-length writes at ->s_maxbytes are OK */
		}

		if (pos + count > inode->i_sb->s_maxbytes)
			count = inode->i_sb->s_maxbytes - pos;
	} else {
		if (is_read_only(inode->i_rdev)) {
			err = -EPERM;
			goto out;
		}
		if (pos >= inode->i_size) {
			if (count || pos > inode->i_size) {
				err = -ENOSPC;
				goto out;
			}
		}

		if (pos + count > inode->i_size)
			count = inode->i_size - pos;
	}

	err = 0;
	if (count == 0)
		goto out;

	remove_suid(file->f_dentry);
	inode->i_ctime = inode->i_mtime = CURRENT_TIME;
	mark_inode_dirty_sync(inode);

	if (file->f_flags & O_DIRECT)
		goto o_direct;

	do {
		unsigned long index, offset;
		long page_fault;
		char *kaddr;

		/*
		 * Try to find the page in the cache. If it isn't there,
		 * allocate a free page.
		 */
		offset = (pos & (PAGE_CACHE_SIZE -1)); /* Within page */
		index = pos >> PAGE_CACHE_SHIFT;
		bytes = PAGE_CACHE_SIZE - offset;
		if (bytes > count)
			bytes = count;

		/*
		 * Bring in the user page that we will copy from _first_.
		 * Otherwise there's a nasty deadlock on copying from the
		 * same page as we're writing to, without it being marked
		 * up-to-date.
		 */
		{ volatile unsigned char dummy;
			__get_user(dummy, buf);
			__get_user(dummy, buf+bytes-1);
		}

		status = -ENOMEM;	/* we'll assign it later anyway */
		page = __grab_cache_page(mapping, index, &cached_page);
		if (!page)
			break;

		/* We have exclusive IO access to the page.. */
		if (!PageLocked(page)) {
			PAGE_BUG(page);
		}

		kaddr = kmap(page);
		status = mapping->a_ops->prepare_write(file, page, offset, offset+bytes);
		if (status)
			goto sync_failure;
		page_fault = __copy_from_user(kaddr+offset, buf, bytes);
		flush_dcache_page(page);
		status = mapping->a_ops->commit_write(file, page, offset, offset+bytes);
		if (page_fault)
			goto fail_write;
		if (!status)
			status = bytes;

		if (status >= 0) {
			written += status;
			count -= status;
			pos += status;
			buf += status;
		}
unlock:
		kunmap(page);
		/* Mark it unlocked again and drop the page.. */
		SetPageReferenced(page);
		unlock_page(page);
		page_cache_release(page);

		if (status < 0)
			break;
		balance_dirty_pages_ratelimited(mapping);
	} while (count);
done:
	*ppos = pos;

	if (cached_page)
		page_cache_release(cached_page);

	/* For now, when the user asks for O_SYNC, we'll actually
	 * provide O_DSYNC. */
	if (status >= 0) {
		if ((file->f_flags & O_SYNC) || IS_SYNC(inode))
			status = generic_osync_inode(inode, OSYNC_METADATA|OSYNC_DATA);
	}
	
out_status:	
	err = written ? written : status;
out:

	up(&inode->i_sem);
	return err;
fail_write:
	status = -EFAULT;
	goto unlock;

sync_failure:
	/*
	 * If blocksize < pagesize, prepare_write() may have instantiated a
	 * few blocks outside i_size.  Trim these off again.
	 */
	kunmap(page);
	unlock_page(page);
	page_cache_release(page);
	if (pos + bytes > inode->i_size)
		vmtruncate(inode, inode->i_size);
	goto done;

o_direct:
	written = generic_file_direct_IO(WRITE, file, (char *) buf, count, pos);
	if (written > 0) {
		loff_t end = pos + written;
		if (end > inode->i_size && !S_ISBLK(inode->i_mode)) {
			inode->i_size = end;
			mark_inode_dirty(inode);
		}
		*ppos = end;
		invalidate_inode_pages2(mapping);
	}
	/*
	 * Sync the fs metadata but not the minor inode changes and
	 * of course not the data as we did direct DMA for the IO.
	 */
	if (written >= 0 && file->f_flags & O_SYNC)
		status = generic_osync_inode(inode, OSYNC_METADATA);
	goto out_status;
}
