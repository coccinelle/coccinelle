/**
 * aops.c - NTFS kernel address space operations and page cache handling.
 * 	    Part of the Linux-NTFS project.
 *
 * Copyright (c) 2001,2002 Anton Altaparmakov.
 * Copyright (C) 2002 Richard Russon.
 *
 * This program/include file is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program/include file is distributed in the hope that it will be 
 * useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
 * of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program (in the main directory of the Linux-NTFS 
 * distribution in the file COPYING); if not, write to the Free Software
 * Foundation,Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <linux/errno.h>
#include <linux/mm.h>
#include <linux/pagemap.h>
#include <linux/swap.h>
#include <linux/locks.h>

#include "ntfs.h"

#define MAX_BUF_PER_PAGE (PAGE_CACHE_SIZE / 512)

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,5,8)
#define page_buffers(page)	(page)->buffers
#endif

/**
 * end_buffer_read_file_async -
 *
 * Async io completion handler for accessing files. Adapted from
 * end_buffer_read_mst_async().
 */
static void end_buffer_read_file_async(struct buffer_head *bh, int uptodate)
{
	static spinlock_t page_uptodate_lock = SPIN_LOCK_UNLOCKED;
	unsigned long flags;
	struct buffer_head *tmp;
	struct page *page;

	mark_buffer_uptodate(bh, uptodate);

	page = bh->b_page;

	if (likely(uptodate)) {
		s64 file_ofs;

		ntfs_inode *ni = NTFS_I(page->mapping->host);

		file_ofs = (page->index << PAGE_CACHE_SHIFT) + bh_offset(bh);
		if (file_ofs + bh->b_size > ni->initialized_size) {
			char *addr;
			int ofs = 0;

			if (file_ofs < ni->initialized_size)
				ofs = ni->initialized_size - file_ofs;
			addr = kmap_atomic(page, KM_BIO_IRQ);
			memset(addr + bh_offset(bh) + ofs, 0, bh->b_size - ofs);
			flush_dcache_page(page);
			kunmap_atomic(addr, KM_BIO_IRQ);
		}
	} else
		SetPageError(page);

	spin_lock_irqsave(&page_uptodate_lock, flags);
	mark_buffer_async(bh, 0);
	unlock_buffer(bh);

	tmp = bh->b_this_page;
	while (tmp != bh) {
		if (buffer_locked(tmp)) {
			if (buffer_async(tmp))
				goto still_busy;
		} else if (!buffer_uptodate(tmp))
			SetPageError(page);
		tmp = tmp->b_this_page;
	}

	spin_unlock_irqrestore(&page_uptodate_lock, flags);
	if (!PageError(page))
		SetPageUptodate(page);
	unlock_page(page);
	return;
still_busy:
	spin_unlock_irqrestore(&page_uptodate_lock, flags);
	return;
}

/**
 * ntfs_file_read_block -
 *
 * NTFS version of block_read_full_page(). Adapted from ntfs_mst_readpage().
 */
static int ntfs_file_read_block(struct page *page)
{
	VCN vcn;
	LCN lcn;
	ntfs_inode *ni;
	ntfs_volume *vol;
	struct buffer_head *bh, *head, *arr[MAX_BUF_PER_PAGE];
	sector_t iblock, lblock, zblock;
	unsigned int blocksize, blocks, vcn_ofs;
	int i, nr;
	unsigned char blocksize_bits;

	ni = NTFS_I(page->mapping->host);
	vol = ni->vol;

	blocksize_bits = VFS_I(ni)->i_blkbits;
	blocksize = 1 << blocksize_bits;

	if (!page_has_buffers(page))
		create_empty_buffers(page, blocksize, 0);
	bh = head = page_buffers(page);
	if (!bh)
		return -ENOMEM;

	blocks = PAGE_CACHE_SIZE >> blocksize_bits;
	iblock = page->index << (PAGE_CACHE_SHIFT - blocksize_bits);
	lblock = (ni->allocated_size + blocksize - 1) >> blocksize_bits;
	zblock = (ni->initialized_size + blocksize - 1) >> blocksize_bits;

#ifdef DEBUG
	if (unlikely(!ni->mft_no)) {
		ntfs_error(vol->sb, "NTFS: Attempt to access $MFT! This is a "
				"very serious bug! Denying access...");
		return -EACCES;
	}
#endif

	/* Loop through all the buffers in the page. */
	nr = i = 0;
	do {
		if (unlikely(buffer_uptodate(bh)))
			continue;
		if (unlikely(buffer_mapped(bh))) {
			arr[nr++] = bh;
			continue;
		}
		bh->b_bdev = vol->sb->s_bdev;
		/* Is the block within the allowed limits? */
		if (iblock < lblock) {
			BOOL is_retry = FALSE;

			/* Convert iblock into corresponding vcn and offset. */
			vcn = (VCN)iblock << blocksize_bits >>
					vol->cluster_size_bits;
			vcn_ofs = ((VCN)iblock << blocksize_bits) &
					vol->cluster_size_mask;
retry_remap:
			/* Convert the vcn to the corresponding lcn. */
			down_read(&ni->run_list.lock);
			lcn = vcn_to_lcn(ni->run_list.rl, vcn);
			up_read(&ni->run_list.lock);
			/* Successful remap. */
			if (lcn >= 0) {
				/* Setup buffer head to correct block. */
				bh->b_blocknr = ((lcn << vol->cluster_size_bits)
						+ vcn_ofs) >> blocksize_bits;
				bh->b_state |= (1UL << BH_Mapped);
				/* Only read initialized data blocks. */
				if (iblock < zblock) {
					arr[nr++] = bh;
					continue;
				}
				/* Fully non-initialized data block, zero it. */
				goto handle_zblock;
			}
			/* It is a hole, need to zero it. */
			if (lcn == LCN_HOLE)
				goto handle_hole;
			/* If first try and run list unmapped, map and retry. */
			if (!is_retry && lcn == LCN_RL_NOT_MAPPED) {
				is_retry = TRUE;
				if (!map_run_list(ni, vcn))
					goto retry_remap;
			}
			/* Hard error, zero out region. */
			SetPageError(page);
			ntfs_error(vol->sb, "vcn_to_lcn(vcn = 0x%Lx) failed "
					"with error code 0x%Lx%s.",
					(long long)vcn, (long long)-lcn,
					is_retry ? " even after retrying" : "");
			// FIXME: Depending on vol->on_errors, do something.
		}
		/*
		 * Either iblock was outside lblock limits or vcn_to_lcn()
		 * returned error. Just zero that portion of the page and set
		 * the buffer uptodate.
		 */
handle_hole:
		bh->b_blocknr = -1UL;
		bh->b_state &= ~(1UL << BH_Mapped);
handle_zblock:
		memset(kmap(page) + i * blocksize, 0, blocksize);
		flush_dcache_page(page);
		kunmap(page);
		set_bit(BH_Uptodate, &bh->b_state);
	} while (i++, iblock++, (bh = bh->b_this_page) != head);

	/* Check we have at least one buffer ready for i/o. */
	if (nr) {
		/* Lock the buffers. */
		for (i = 0; i < nr; i++) {
			struct buffer_head *tbh = arr[i];
			lock_buffer(tbh);
			tbh->b_end_io = end_buffer_read_file_async;
			mark_buffer_async(tbh, 1);
		}
		/* Finally, start i/o on the buffers. */
		for (i = 0; i < nr; i++)
			submit_bh(READ, arr[i]);
		return 0;
	}
	/* No i/o was scheduled on any of the buffers. */
	if (!PageError(page))
		SetPageUptodate(page);
	else /* Signal synchronous i/o error. */
		nr = -EIO;
	unlock_page(page);
	return nr;
}

/**
 * ntfs_file_readpage - fill a @page of a @file with data from the device
 * @file:	open file to which the page @page belongs or NULL
 * @page:	page cache page to fill with data
 *
 * For non-resident attributes, ntfs_file_readpage() fills the @page of the open
 * file @file by calling the generic block_read_full_page() function provided by
 * the kernel which in turn invokes our ntfs_file_get_block() callback in order
 * to create and read in the buffers associated with the page asynchronously.
 *
 * For resident attributes, OTOH, ntfs_file_readpage() fills @page by copying
 * the data from the mft record (which at this stage is most likely in memory)
 * and fills the remainder with zeroes. Thus, in this case I/O is synchronous,
 * as even if the mft record is not cached at this point in time, we need to
 * wait for it to be read in before we can do the copy.
 *
 * Return zero on success or -errno on error.
 */
static int ntfs_file_readpage(struct file *file, struct page *page)
{
	s64 attr_pos;
	ntfs_inode *ni;
	char *addr;
	attr_search_context *ctx;
	MFT_RECORD *mrec;
	u32 attr_len;
	int err = 0;

	if (!PageLocked(page))
		PAGE_BUG(page);

	ni = NTFS_I(page->mapping->host);

	/* Is the unnamed $DATA attribute resident? */
	if (test_bit(NI_NonResident, &ni->state)) {
		/* Attribute is not resident. */

		/* If the file is encrypted, we deny access, just like NT4. */
		if (test_bit(NI_Encrypted, &ni->state)) {
			err = -EACCES;
			goto unl_err_out;
		}
		/* Compressed data stream. Handled in compress.c. */
		if (test_bit(NI_Compressed, &ni->state))
			return ntfs_file_read_compressed_block(page);
		/* Normal data stream. */
		return ntfs_file_read_block(page);
	}
	/* Attribute is resident, implying it is not compressed or encrypted. */

	/* Map, pin and lock the mft record for reading. */
	mrec = map_mft_record(READ, ni);
	if (IS_ERR(mrec)) {
		err = PTR_ERR(mrec);
		goto unl_err_out;
	}

	ctx = get_attr_search_ctx(ni, mrec);
	if (!ctx) {
		err = -ENOMEM;
		goto unm_unl_err_out;
	}

	/* Find the data attribute in the mft record. */
	if (!lookup_attr(AT_DATA, NULL, 0, 0, 0, NULL, 0, ctx)) {
		err = -ENOENT;
		goto put_unm_unl_err_out;
	}

	/* Starting position of the page within the attribute value. */
	attr_pos = page->index << PAGE_CACHE_SHIFT;

	/* The total length of the attribute value. */
	attr_len = le32_to_cpu(ctx->attr->_ARA(value_length));

	addr = kmap(page);
	/* Copy over in bounds data, zeroing the remainder of the page. */
	if (attr_pos < attr_len) {
		u32 bytes = attr_len - attr_pos;
		if (bytes > PAGE_CACHE_SIZE)
			bytes = PAGE_CACHE_SIZE;
		else if (bytes < PAGE_CACHE_SIZE)
			memset(addr + bytes, 0, PAGE_CACHE_SIZE - bytes);
		/* Copy the data to the page. */
		memcpy(addr, attr_pos + (char*)ctx->attr +
				le16_to_cpu(ctx->attr->_ARA(value_offset)),
				bytes);
	} else
		memset(addr, 0, PAGE_CACHE_SIZE);
	flush_dcache_page(page);
	kunmap(page);

	SetPageUptodate(page);
put_unm_unl_err_out:
	put_attr_search_ctx(ctx);
unm_unl_err_out:
	unmap_mft_record(READ, ni);
unl_err_out:
	unlock_page(page);
	return err;
}

/**
 * end_buffer_read_mftbmp_async -
 *
 * Async io completion handler for accessing mft bitmap. Adapted from
 * end_buffer_read_mst_async().
 */
static void end_buffer_read_mftbmp_async(struct buffer_head *bh, int uptodate)
{
	static spinlock_t page_uptodate_lock = SPIN_LOCK_UNLOCKED;
	unsigned long flags;
	struct buffer_head *tmp;
	struct page *page;

	mark_buffer_uptodate(bh, uptodate);

	page = bh->b_page;

	if (likely(uptodate)) {
		s64 file_ofs;

		/* Host is the ntfs volume. Our mft bitmap access kludge... */
		ntfs_volume *vol = (ntfs_volume*)page->mapping->host;

		file_ofs = (page->index << PAGE_CACHE_SHIFT) + bh_offset(bh);
		if (file_ofs + bh->b_size > vol->mftbmp_initialized_size) {
			char *addr;
			int ofs = 0;

			if (file_ofs < vol->mftbmp_initialized_size)
				ofs = vol->mftbmp_initialized_size - file_ofs;
			addr = kmap_atomic(page, KM_BIO_IRQ);
			memset(addr + bh_offset(bh) + ofs, 0, bh->b_size - ofs);
			flush_dcache_page(page);
			kunmap_atomic(addr, KM_BIO_IRQ);
		}
	} else
		SetPageError(page);

	spin_lock_irqsave(&page_uptodate_lock, flags);
	mark_buffer_async(bh, 0);
	unlock_buffer(bh);

	tmp = bh->b_this_page;
	while (tmp != bh) {
		if (buffer_locked(tmp)) {
			if (buffer_async(tmp))
				goto still_busy;
		} else if (!buffer_uptodate(tmp))
			SetPageError(page);
		tmp = tmp->b_this_page;
	}

	spin_unlock_irqrestore(&page_uptodate_lock, flags);
	if (!PageError(page))
		SetPageUptodate(page);
	unlock_page(page);
	return;
still_busy:
	spin_unlock_irqrestore(&page_uptodate_lock, flags);
	return;
}

/**
 * ntfs_mftbmp_readpage -
 *
 * Readpage for accessing mft bitmap. Adapted from ntfs_mst_readpage().
 */
static int ntfs_mftbmp_readpage(ntfs_volume *vol, struct page *page)
{
	VCN vcn;
	LCN lcn;
	struct buffer_head *bh, *head, *arr[MAX_BUF_PER_PAGE];
	sector_t iblock, lblock, zblock;
	unsigned int blocksize, blocks, vcn_ofs;
	int nr, i;
	unsigned char blocksize_bits;

	if (!PageLocked(page))
		PAGE_BUG(page);

	blocksize = vol->sb->s_blocksize;
	blocksize_bits = vol->sb->s_blocksize_bits;

	if (!page_has_buffers(page))
		create_empty_buffers(page, blocksize, 0);
	bh = head = page_buffers(page);
	if (!bh)
		return -ENOMEM;

	blocks = PAGE_CACHE_SIZE >> blocksize_bits;
	iblock = page->index << (PAGE_CACHE_SHIFT - blocksize_bits);
	lblock = (vol->mftbmp_allocated_size + blocksize - 1) >> blocksize_bits;
	zblock = (vol->mftbmp_initialized_size + blocksize - 1) >>
			blocksize_bits;

	/* Loop through all the buffers in the page. */
	nr = i = 0;
	do {
		if (unlikely(buffer_uptodate(bh)))
			continue;
		if (unlikely(buffer_mapped(bh))) {
			arr[nr++] = bh;
			continue;
		}
		bh->b_bdev = vol->sb->s_bdev;
		/* Is the block within the allowed limits? */
		if (iblock < lblock) {
			/* Convert iblock into corresponding vcn and offset. */
			vcn = (VCN)iblock << blocksize_bits >>
					vol->cluster_size_bits;
			vcn_ofs = ((VCN)iblock << blocksize_bits) &
					vol->cluster_size_mask;
			/* Convert the vcn to the corresponding lcn. */
			down_read(&vol->mftbmp_rl.lock);
			lcn = vcn_to_lcn(vol->mftbmp_rl.rl, vcn);
			up_read(&vol->mftbmp_rl.lock);
			/* Successful remap. */
			if (lcn >= 0) {
				/* Setup buffer head to correct block. */
				bh->b_blocknr = ((lcn << vol->cluster_size_bits)
						+ vcn_ofs) >> blocksize_bits;
				bh->b_state |= (1UL << BH_Mapped);
				/* Only read initialized data blocks. */
				if (iblock < zblock) {
					arr[nr++] = bh;
					continue;
				}
				/* Fully non-initialized data block, zero it. */
				goto handle_zblock;
			}
			if (lcn != LCN_HOLE) {
				/* Hard error, zero out region. */
				SetPageError(page);
				ntfs_error(vol->sb, "vcn_to_lcn(vcn = 0x%Lx) "
						"failed with error code "
						"0x%Lx.", (long long)vcn,
						(long long)-lcn);
				// FIXME: Depending on vol->on_errors, do
				// something.
			}
		}
		/*
		 * Either iblock was outside lblock limits or vcn_to_lcn()
		 * returned error. Just zero that portion of the page and set
		 * the buffer uptodate.
		 */
		bh->b_blocknr = -1UL;
		bh->b_state &= ~(1UL << BH_Mapped);
handle_zblock:
		memset(kmap(page) + i * blocksize, 0, blocksize);
		flush_dcache_page(page);
		kunmap(page);
		set_bit(BH_Uptodate, &bh->b_state);
	} while (i++, iblock++, (bh = bh->b_this_page) != head);

	/* Check we have at least one buffer ready for i/o. */
	if (nr) {
		/* Lock the buffers. */
		for (i = 0; i < nr; i++) {
			struct buffer_head *tbh = arr[i];
			lock_buffer(tbh);
			tbh->b_end_io = end_buffer_read_mftbmp_async;
			mark_buffer_async(tbh, 1);
		}
		/* Finally, start i/o on the buffers. */
		for (i = 0; i < nr; i++)
			submit_bh(READ, arr[i]);
		return 0;
	}
	/* No i/o was scheduled on any of the buffers. */
	if (!PageError(page))
		SetPageUptodate(page);
	else /* Signal synchronous i/o error. */
		nr = -EIO;
	unlock_page(page);
	return nr;
}

/**
 * end_buffer_read_mst_async - async io completion for reading index records
 * @bh:		buffer head on which io is completed
 * @uptodate:	whether @bh is now uptodate or not
 *
 * Asynchronous I/O completion handler for reading pages belonging to the
 * index allocation attribute address space of directory inodes.
 *
 * Perform the post read mst fixups when all IO on the page has been completed
 * and marks the page uptodate or sets the error bit on the page.
 *
 * Adapted from fs/buffer.c.
 *
 * NOTE: We use this function as async io completion handler for reading pages
 * belonging to the mft data attribute address space, too as this saves
 * duplicating an almost identical function. We do this by cheating a little
 * bit in setting the index_block_size in the mft ntfs_inode to the mft record
 * size of the volume (vol->mft_record_size), and index_block_size_bits to
 * mft_record_size_bits, respectively.
 */
static void end_buffer_read_mst_async(struct buffer_head *bh, int uptodate)
{
	static spinlock_t page_uptodate_lock = SPIN_LOCK_UNLOCKED;
	unsigned long flags;
	struct buffer_head *tmp;
	struct page *page;
	ntfs_inode *ni;

	mark_buffer_uptodate(bh, uptodate);

	page = bh->b_page;

	ni = NTFS_I(page->mapping->host);

	if (likely(uptodate)) {
		s64 file_ofs;

		file_ofs = (page->index << PAGE_CACHE_SHIFT) + bh_offset(bh);
		/* Check for the current buffer head overflowing. */
		if (file_ofs + bh->b_size > ni->initialized_size) {
			char *addr;
			int ofs = 0;

			if (file_ofs < ni->initialized_size)
				ofs = ni->initialized_size - file_ofs;
			addr = kmap_atomic(page, KM_BIO_IRQ);
			memset(addr + bh_offset(bh) + ofs, 0, bh->b_size - ofs);
			flush_dcache_page(page);
			kunmap_atomic(addr, KM_BIO_IRQ);
		}
	} else
		SetPageError(page);

	spin_lock_irqsave(&page_uptodate_lock, flags);
	mark_buffer_async(bh, 0);
	unlock_buffer(bh);

	tmp = bh->b_this_page;
	while (tmp != bh) {
		if (buffer_locked(tmp)) {
			if (buffer_async(tmp))
				goto still_busy;
		} else if (!buffer_uptodate(tmp))
			SetPageError(page);
		tmp = tmp->b_this_page;
	}

	spin_unlock_irqrestore(&page_uptodate_lock, flags);
	/*
	 * If none of the buffers had errors then we can set the page uptodate,
	 * but we first have to perform the post read mst fixups.
	 */
	if (!PageError(page)) {
		char *addr;
		unsigned int i, recs, nr_err = 0;
		u32 rec_size;

		rec_size = ni->_IDM(index_block_size);
		recs = PAGE_CACHE_SIZE / rec_size;
		addr = kmap_atomic(page, KM_BIO_IRQ);
		for (i = 0; i < recs; i++) {
			if (!post_read_mst_fixup((NTFS_RECORD*)(addr +
					i * rec_size), rec_size))
				continue;
			nr_err++;
			ntfs_error(ni->vol->sb, "post_read_mst_fixup() failed, "
					"corrupt %s record 0x%Lx. Run chkdsk.",
					ni->mft_no ? "index" : "mft",
					(long long)((page->index <<
					PAGE_CACHE_SHIFT >>
					ni->_IDM(index_block_size_bits)) + i));
		}
		flush_dcache_page(page);
		kunmap_atomic(addr, KM_BIO_IRQ);
		if (likely(!nr_err && recs))
			SetPageUptodate(page);
		else {
			ntfs_error(ni->vol->sb, "Setting page error, index "
					"0x%lx.", page->index);
			SetPageError(page);
		}
	}
	unlock_page(page);
	return;
still_busy:
	spin_unlock_irqrestore(&page_uptodate_lock, flags);
	return;
}

/**
 * ntfs_mst_readpage - fill a @page of the mft or a directory with data
 * @file:	open file/directory to which the page @page belongs or NULL
 * @page:	page cache page to fill with data
 *
 * Readpage method for the VFS address space operations.
 *
 * Fill the page @page of the $MFT or the open directory @dir. We read each
 * buffer asynchronously and when all buffers are read in our io completion
 * handler end_buffer_read_mst_async() automatically applies the mst fixups to
 * the page before finally marking it uptodate and unlocking it.
 *
 * Contains an adapted version of fs/buffer.c::block_read_full_page().
 */
int ntfs_mst_readpage(struct file *dir, struct page *page)
{
	VCN vcn;
	LCN lcn;
	ntfs_inode *ni;
	ntfs_volume *vol;
	struct buffer_head *bh, *head, *arr[MAX_BUF_PER_PAGE];
	sector_t iblock, lblock, zblock;
	unsigned int blocksize, blocks, vcn_ofs;
	int i, nr;
	unsigned char blocksize_bits;

	if (!PageLocked(page))
		PAGE_BUG(page);

	ni = NTFS_I(page->mapping->host);
	vol = ni->vol;

	blocksize_bits = VFS_I(ni)->i_blkbits;
	blocksize = 1 << blocksize_bits;

	if (!page_has_buffers(page))
		create_empty_buffers(page, blocksize, 0);
	bh = head = page_buffers(page);
	if (!bh)
		return -ENOMEM;

	blocks = PAGE_CACHE_SIZE >> blocksize_bits;
	iblock = page->index << (PAGE_CACHE_SHIFT - blocksize_bits);
	lblock = (ni->allocated_size + blocksize - 1) >> blocksize_bits;
	zblock = (ni->initialized_size + blocksize - 1) >> blocksize_bits;

#ifdef DEBUG
	if (unlikely(!ni->run_list.rl && !ni->mft_no))
		panic("NTFS: $MFT/$DATA run list has been unmapped! This is a "
				"very serious bug! Cannot continue...");
#endif

	/* Loop through all the buffers in the page. */
	nr = i = 0;
	do {
		if (unlikely(buffer_uptodate(bh)))
			continue;
		if (unlikely(buffer_mapped(bh))) {
			arr[nr++] = bh;
			continue;
		}
		bh->b_bdev = vol->sb->s_bdev;
		/* Is the block within the allowed limits? */
		if (iblock < lblock) {
			BOOL is_retry = FALSE;

			/* Convert iblock into corresponding vcn and offset. */
			vcn = (VCN)iblock << blocksize_bits >>
					vol->cluster_size_bits;
			vcn_ofs = ((VCN)iblock << blocksize_bits) &
					vol->cluster_size_mask;
retry_remap:
			/* Convert the vcn to the corresponding lcn. */
			down_read(&ni->run_list.lock);
			lcn = vcn_to_lcn(ni->run_list.rl, vcn);
			up_read(&ni->run_list.lock);
			/* Successful remap. */
			if (lcn >= 0) {
				/* Setup buffer head to correct block. */
				bh->b_blocknr = ((lcn << vol->cluster_size_bits)
						+ vcn_ofs) >> blocksize_bits;
				bh->b_state |= (1UL << BH_Mapped);
				/* Only read initialized data blocks. */
				if (iblock < zblock) {
					arr[nr++] = bh;
					continue;
				}
				/* Fully non-initialized data block, zero it. */
				goto handle_zblock;
			}
			/* It is a hole, need to zero it. */
			if (lcn == LCN_HOLE)
				goto handle_hole;
			/* If first try and run list unmapped, map and retry. */
			if (!is_retry && lcn == LCN_RL_NOT_MAPPED) {
				is_retry = TRUE;
				if (!map_run_list(ni, vcn))
					goto retry_remap;
			}
			/* Hard error, zero out region. */
			SetPageError(page);
			ntfs_error(vol->sb, "vcn_to_lcn(vcn = 0x%Lx) failed "
					"with error code 0x%Lx%s.",
					(long long)vcn, (long long)-lcn,
					is_retry ? " even after retrying" : "");
			// FIXME: Depending on vol->on_errors, do something.
		}
		/*
		 * Either iblock was outside lblock limits or vcn_to_lcn()
		 * returned error. Just zero that portion of the page and set
		 * the buffer uptodate.
		 */
handle_hole:
		bh->b_blocknr = -1UL;
		bh->b_state &= ~(1UL << BH_Mapped);
handle_zblock:
		memset(kmap(page) + i * blocksize, 0, blocksize);
		flush_dcache_page(page);
		kunmap(page);
		set_bit(BH_Uptodate, &bh->b_state);
	} while (i++, iblock++, (bh = bh->b_this_page) != head);

	/* Check we have at least one buffer ready for i/o. */
	if (nr) {
		/* Lock the buffers. */
		for (i = 0; i < nr; i++) {
			struct buffer_head *tbh = arr[i];
			lock_buffer(tbh);
			tbh->b_end_io = end_buffer_read_mst_async;
			mark_buffer_async(tbh, 1);
		}
		/* Finally, start i/o on the buffers. */
		for (i = 0; i < nr; i++)
			submit_bh(READ, arr[i]);
		return 0;
	}
	/* No i/o was scheduled on any of the buffers. */
	if (!PageError(page))
		SetPageUptodate(page);
	else /* Signal synchronous i/o error. */
		nr = -EIO;
	unlock_page(page);
	return nr;
}

/**
 * ntfs_file_aops - address space operations for accessing normal file data
 */
struct address_space_operations ntfs_file_aops = {
	writepage:	NULL,			/* Write dirty page to disk. */
	readpage:	ntfs_file_readpage,	/* Fill page with data. */
	sync_page:	block_sync_page,	/* Currently, just unplugs the
						   disk request queue. */
	prepare_write:	NULL,			/* . */
	commit_write:	NULL,			/* . */
};

typedef int readpage_t(struct file *, struct page *);

/**
 * ntfs_mftbmp_aops - address space operations for accessing mftbmp
 */
struct address_space_operations ntfs_mftbmp_aops = {
	writepage:	NULL,			/* Write dirty page to disk. */
	readpage:	(readpage_t*)ntfs_mftbmp_readpage, /* Fill page with
							      data. */
	sync_page:	block_sync_page,	/* Currently, just unplugs the
						   disk request queue. */
	prepare_write:	NULL,			/* . */
	commit_write:	NULL,			/* . */
};

/**
 * ntfs_dir_aops -
 *
 * Address space operations for accessing normal directory data (i.e. index
 * allocation attribute). We can't just use the same operations as for files
 * because 1) the attribute is different and even more importantly 2) the index
 * records have to be multi sector transfer deprotected (i.e. fixed-up).
 */
struct address_space_operations ntfs_dir_aops = {
	writepage:	NULL,			/* Write dirty page to disk. */
	readpage:	ntfs_mst_readpage,	/* Fill page with data. */
	sync_page:	block_sync_page,	/* Currently, just unplugs the
						   disk request queue. */
	prepare_write:	NULL,			/* . */
	commit_write:	NULL,			/* . */
};

