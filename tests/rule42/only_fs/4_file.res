/*
 * JFFS2 -- Journalling Flash File System, Version 2.
 *
 * Copyright (C) 2001, 2002 Red Hat, Inc.
 *
 * Created by David Woodhouse <dwmw2@cambridge.redhat.com>
 *
 * The original JFFS, from which the design for JFFS2 was derived,
 * was designed and implemented by Axis Communications AB.
 *
 * The contents of this file are subject to the Red Hat eCos Public
 * License Version 1.1 (the "Licence"); you may not use this file
 * except in compliance with the Licence.  You may obtain a copy of
 * the Licence at http://www.redhat.com/
 *
 * Software distributed under the Licence is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
 * See the Licence for the specific language governing rights and
 * limitations under the Licence.
 *
 * The Original Code is JFFS2 - Journalling Flash File System, version 2
 *
 * Alternatively, the contents of this file may be used under the
 * terms of the GNU General Public License version 2 (the "GPL"), in
 * which case the provisions of the GPL are applicable instead of the
 * above.  If you wish to allow the use of your version of this file
 * only under the terms of the GPL and not to allow others to use your
 * version of this file under the RHEPL, indicate your decision by
 * deleting the provisions above and replace them with the notice and
 * other provisions required by the GPL.  If you do not delete the
 * provisions above, a recipient may use your version of this file
 * under either the RHEPL or the GPL.
 *
 * 
 *
 */

#include <linux/kernel.h>
#include <linux/mtd/compatmac.h> /* for min() */
#include <linux/slab.h>
#include <linux/fs.h>
#include <linux/time.h>
#include <linux/pagemap.h>
#include <linux/crc32.h>
#include <linux/jffs2.h>
#include <linux/smp_lock.h>
#include "nodelist.h"

extern int generic_file_open(struct inode *, struct file *) __attribute__((weak));
extern loff_t generic_file_llseek(struct file *file, loff_t offset, int origin) __attribute__((weak));


int jffs2_fsync(struct file *filp, struct dentry *dentry, int datasync)
{
	struct inode *inode = dentry->d_inode;
	struct jffs2_inode_info *f = JFFS2_INODE_INFO(inode);
	struct jffs2_sb_info *c = JFFS2_SB_INFO(inode->i_sb);
	if (!c->wbuf || !c->wbuf_len)
		return 0;

	/* flush write buffer and update c->nextblock */
	
	/* FIXME NAND */
	/* At the moment we flush the buffer, to make sure
	 * that every thing is on the flash.
	 * maybe we have to think about it to find a smarter
	 * solution.
	 */
	down(&f->sem);
	jffs2_flush_wbuf(c,2);
	up(&f->sem);
			
	return 0;	
}

struct file_operations jffs2_file_operations =
{
	llseek:		generic_file_llseek,
	open:		generic_file_open,
	read:		generic_file_read,
	write:		generic_file_write,
	ioctl:		jffs2_ioctl,
	mmap:		generic_file_mmap,
	fsync:		jffs2_fsync
};

/* jffs2_file_inode_operations */

struct inode_operations jffs2_file_inode_operations =
{
	setattr:	jffs2_setattr
};

struct address_space_operations jffs2_file_address_operations =
{
	readpage:	jffs2_readpage,
	prepare_write:	jffs2_prepare_write,
	commit_write:	jffs2_commit_write
};

int jffs2_setattr (struct dentry *dentry, struct iattr *iattr)
{
	struct jffs2_full_dnode *old_metadata, *new_metadata;
	struct inode *inode = dentry->d_inode;
	struct jffs2_inode_info *f = JFFS2_INODE_INFO(inode);
	struct jffs2_sb_info *c = JFFS2_SB_INFO(inode->i_sb);
	struct jffs2_raw_inode *ri;
	unsigned short dev;
	unsigned char *mdata = NULL;
	int mdatalen = 0;
	unsigned int ivalid;
	uint32_t phys_ofs, alloclen;
	int ret = 0;
	lock_kernel();
	D1(printk(KERN_DEBUG "jffs2_setattr(): ino #%lu\n", inode->i_ino));
	ret = inode_change_ok(inode, iattr);
	if (ret) 
		goto out;

	/* Special cases - we don't want more than one data node
	   for these types on the medium at any time. So setattr
	   must read the original data associated with the node
	   (i.e. the device numbers or the target name) and write
	   it out again with the appropriate data attached */
	if (S_ISBLK(inode->i_mode) || S_ISCHR(inode->i_mode)) {
		/* For these, we don't actually need to read the old node */
		dev =  (major(dentry->d_inode->i_rdev) << 8) | 
			minor(dentry->d_inode->i_rdev);
		mdata = (char *)&dev;
		mdatalen = sizeof(dev);
		D1(printk(KERN_DEBUG "jffs2_setattr(): Writing %d bytes of kdev_t\n", mdatalen));
	} else if (S_ISLNK(inode->i_mode)) {
		mdatalen = f->metadata->size;
		mdata = kmalloc(f->metadata->size, GFP_USER);
		if (!mdata) {
			ret = -ENOMEM;
			goto out;
		}
		ret = jffs2_read_dnode(c, f->metadata, mdata, 0, mdatalen);
		if (ret) {
			kfree(mdata);
			goto out;
		}
		D1(printk(KERN_DEBUG "jffs2_setattr(): Writing %d bytes of symlink target\n", mdatalen));
	}

	ri = jffs2_alloc_raw_inode();
	if (!ri) {
		if (S_ISLNK(inode->i_mode))
			kfree(mdata);
		ret = -ENOMEM;
		goto out;
	}
		
	ret = jffs2_reserve_space(c, sizeof(*ri) + mdatalen, &phys_ofs, &alloclen, ALLOC_NORMAL);
	if (ret) {
		jffs2_free_raw_inode(ri);
		if (S_ISLNK(inode->i_mode & S_IFMT))
			 kfree(mdata);
		goto out;
	}
	down(&f->sem);
	ivalid = iattr->ia_valid;
	
	ri->magic = JFFS2_MAGIC_BITMASK;
	ri->nodetype = JFFS2_NODETYPE_INODE;
	ri->totlen = sizeof(*ri) + mdatalen;
	ri->hdr_crc = crc32(0, ri, sizeof(struct jffs2_unknown_node)-4);

	ri->ino = inode->i_ino;
	ri->version = ++f->highest_version;

	ri->mode = (ivalid & ATTR_MODE)?iattr->ia_mode:inode->i_mode;
	ri->uid = (ivalid & ATTR_UID)?iattr->ia_uid:inode->i_uid;
	ri->gid = (ivalid & ATTR_GID)?iattr->ia_gid:inode->i_gid;

	if (ivalid & ATTR_MODE && ri->mode & S_ISGID &&
	    !in_group_p(ri->gid) && !capable(CAP_FSETID))
		ri->mode &= ~S_ISGID;

	ri->isize = (ivalid & ATTR_SIZE)?iattr->ia_size:inode->i_size;
	ri->atime = (ivalid & ATTR_ATIME)?iattr->ia_atime:inode->i_atime;
	ri->mtime = (ivalid & ATTR_MTIME)?iattr->ia_mtime:inode->i_mtime;
	ri->ctime = (ivalid & ATTR_CTIME)?iattr->ia_ctime:inode->i_ctime;

	ri->offset = 0;
	ri->csize = ri->dsize = mdatalen;
	ri->compr = JFFS2_COMPR_NONE;
	if (inode->i_size < ri->isize) {
		/* It's an extension. Make it a hole node */
		ri->compr = JFFS2_COMPR_ZERO;
		ri->dsize = ri->isize - inode->i_size;
		ri->offset = inode->i_size;
	}
	ri->node_crc = crc32(0, ri, sizeof(*ri)-8);
	if (mdatalen)
		ri->data_crc = crc32(0, mdata, mdatalen);
	else
		ri->data_crc = 0;

	new_metadata = jffs2_write_dnode(c, f, ri, mdata, mdatalen, phys_ofs, NULL);
	if (S_ISLNK(inode->i_mode))
		kfree(mdata);
	
	if (IS_ERR(new_metadata)) {
		jffs2_complete_reservation(c);
		jffs2_free_raw_inode(ri);
		up(&f->sem);
		ret = PTR_ERR(new_metadata);
		goto out;
	}
	/* It worked. Update the inode */
	inode->i_atime = ri->atime;
	inode->i_ctime = ri->ctime;
	inode->i_mtime = ri->mtime;
	inode->i_mode = ri->mode;
	inode->i_uid = ri->uid;
	inode->i_gid = ri->gid;


	old_metadata = f->metadata;

	if (inode->i_size > ri->isize) {
		vmtruncate(inode, ri->isize);
		jffs2_truncate_fraglist (c, &f->fraglist, ri->isize);
	}

	if (inode->i_size < ri->isize) {
		jffs2_add_full_dnode_to_inode(c, f, new_metadata);
		inode->i_size = ri->isize;
		f->metadata = NULL;
	} else {
		f->metadata = new_metadata;
	}
	if (old_metadata) {
		jffs2_mark_node_obsolete(c, old_metadata->raw);
		jffs2_free_full_dnode(old_metadata);
	}
	jffs2_free_raw_inode(ri);

	up(&f->sem);
	jffs2_complete_reservation(c);

out:
	unlock_kernel();	
	return ret;
}

int jffs2_do_readpage_nolock (struct inode *inode, struct page *pg)
{
	struct jffs2_inode_info *f = JFFS2_INODE_INFO(inode);
	struct jffs2_sb_info *c = JFFS2_SB_INFO(inode->i_sb);
	unsigned char *pg_buf;
	int ret;

	D1(printk(KERN_DEBUG "jffs2_do_readpage_nolock(): ino #%lu, page at offset 0x%lx\n", inode->i_ino, pg->index << PAGE_CACHE_SHIFT));

	if (!PageLocked(pg))
                PAGE_BUG(pg);

	pg_buf = kmap(pg);
	/* FIXME: Can kmap fail? */

	ret = jffs2_read_inode_range(c, f, pg_buf, pg->index << PAGE_CACHE_SHIFT, PAGE_CACHE_SIZE);

	if (ret) {
		ClearPageUptodate(pg);
		SetPageError(pg);
	} else {
		SetPageUptodate(pg);
		ClearPageError(pg);
	}

	flush_dcache_page(pg);
	kunmap(pg);

	D1(printk(KERN_DEBUG "readpage finished\n"));
	return 0;
}

int jffs2_do_readpage_unlock(struct inode *inode, struct page *pg)
{
	int ret = jffs2_do_readpage_nolock(inode, pg);
	unlock_page(pg);
	return ret;
}


int jffs2_readpage (struct file *filp, struct page *pg)
{
	struct jffs2_inode_info *f = JFFS2_INODE_INFO(filp->f_dentry->d_inode);
	int ret;
	
	down(&f->sem);
	ret = jffs2_do_readpage_unlock(filp->f_dentry->d_inode, pg);
	up(&f->sem);
	return ret;
}

int jffs2_prepare_write (struct file *filp, struct page *pg, unsigned start, unsigned end)
{
	struct inode *inode = filp->f_dentry->d_inode;
	struct jffs2_inode_info *f = JFFS2_INODE_INFO(inode);
	uint32_t pageofs = pg->index << PAGE_CACHE_SHIFT;
	int ret = 0;

	down(&f->sem);
	D1(printk(KERN_DEBUG "jffs2_prepare_write()\n"));

	if (pageofs > inode->i_size) {
		/* Make new hole frag from old EOF to new page */
		struct jffs2_sb_info *c = JFFS2_SB_INFO(inode->i_sb);
		struct jffs2_raw_inode ri;
		struct jffs2_full_dnode *fn;
		uint32_t phys_ofs, alloc_len;
		
		D1(printk(KERN_DEBUG "Writing new hole frag 0x%x-0x%x between current EOF and new page\n",
			  (unsigned int)inode->i_size, pageofs));

		ret = jffs2_reserve_space(c, sizeof(ri), &phys_ofs, &alloc_len, ALLOC_NORMAL);
		if (ret) {
			up(&f->sem);
			return ret;
		}
		memset(&ri, 0, sizeof(ri));

		ri.magic = JFFS2_MAGIC_BITMASK;
		ri.nodetype = JFFS2_NODETYPE_INODE;
		ri.totlen = sizeof(ri);
		ri.hdr_crc = crc32(0, &ri, sizeof(struct jffs2_unknown_node)-4);

		ri.ino = f->inocache->ino;
		ri.version = ++f->highest_version;
		ri.mode = inode->i_mode;
		ri.uid = inode->i_uid;
		ri.gid = inode->i_gid;
		ri.isize = max((uint32_t)inode->i_size, pageofs);
		ri.atime = ri.ctime = ri.mtime = CURRENT_TIME;
		ri.offset = inode->i_size;
		ri.dsize = pageofs - inode->i_size;
		ri.csize = 0;
		ri.compr = JFFS2_COMPR_ZERO;
		ri.node_crc = crc32(0, &ri, sizeof(ri)-8);
		ri.data_crc = 0;
		
		fn = jffs2_write_dnode(c, f, &ri, NULL, 0, phys_ofs, NULL);

		if (IS_ERR(fn)) {
			ret = PTR_ERR(fn);
			jffs2_complete_reservation(c);
			up(&f->sem);
			return ret;
		}
		ret = jffs2_add_full_dnode_to_inode(c, f, fn);
		if (f->metadata) {
			jffs2_mark_node_obsolete(c, f->metadata->raw);
			jffs2_free_full_dnode(f->metadata);
			f->metadata = NULL;
		}
		if (ret) {
			D1(printk(KERN_DEBUG "Eep. add_full_dnode_to_inode() failed in prepare_write, returned %d\n", ret));
			jffs2_mark_node_obsolete(c, fn->raw);
			jffs2_free_full_dnode(fn);
			jffs2_complete_reservation(c);
			up(&f->sem);
			return ret;
		}
		jffs2_complete_reservation(c);
		inode->i_size = pageofs;
	}
	

	/* Read in the page if it wasn't already present */
	if (!PageUptodate(pg) && (start || end < PAGE_SIZE))
		ret = jffs2_do_readpage_nolock(inode, pg);
	D1(printk(KERN_DEBUG "end prepare_write()\n"));
	up(&f->sem);
	return ret;
}

int jffs2_commit_write (struct file *filp, struct page *pg, unsigned start, unsigned end)
{
	/* Actually commit the write from the page cache page we're looking at.
	 * For now, we write the full page out each time. It sucks, but it's simple
	 */
	struct inode *inode = filp->f_dentry->d_inode;
	struct jffs2_inode_info *f = JFFS2_INODE_INFO(inode);
	struct jffs2_sb_info *c = JFFS2_SB_INFO(inode->i_sb);
	struct jffs2_raw_inode *ri;
	int ret = 0;
	uint32_t writtenlen = 0;

	D1(printk(KERN_DEBUG "jffs2_commit_write(): ino #%lu, page at 0x%lx, range %d-%d\n",
		  inode->i_ino, pg->index << PAGE_CACHE_SHIFT, start, end));

	ri = jffs2_alloc_raw_inode();

	if (!ri) {
		D1(printk(KERN_DEBUG "jffs2_commit_write(): Allocation of raw inode failed\n"));
		return -ENOMEM;
	}

	/* Set the fields that the generic jffs2_write_inode_range() code can't find */
	ri->ino = inode->i_ino;
	ri->mode = inode->i_mode;
	ri->uid = inode->i_uid;
	ri->gid = inode->i_gid;
	ri->isize = (uint32_t)inode->i_size;
	ri->atime = ri->ctime = ri->mtime = CURRENT_TIME;

	/* We rely on the fact that generic_file_write() currently kmaps the page for us. */
	ret = jffs2_write_inode_range(c, f, ri, page_address(pg) + start,
				      (pg->index << PAGE_CACHE_SHIFT) + start, end - start, &writtenlen);

	if (ret) {
		/* There was an error writing. */
		SetPageError(pg);
	}

	if (writtenlen) {
		if (inode->i_size < (pg->index << PAGE_CACHE_SHIFT) + start + writtenlen) {
			inode->i_size = (pg->index << PAGE_CACHE_SHIFT) + start + writtenlen;
			inode->i_blocks = (inode->i_size + 511) >> 9;
			
			inode->i_ctime = inode->i_mtime = ri->ctime;
		}
	}

	jffs2_free_raw_inode(ri);

	if (start+writtenlen < end) {
		/* generic_file_write has written more to the page cache than we've
		   actually written to the medium. Mark the page !Uptodate so that 
		   it gets reread */
		D1(printk(KERN_DEBUG "jffs2_commit_write(): Not all bytes written. Marking page !uptodate\n"));
		SetPageError(pg);
		ClearPageUptodate(pg);
	}

	D1(printk(KERN_DEBUG "jffs2_commit_write() returning %d\n",writtenlen?writtenlen:ret));
	return writtenlen?writtenlen:ret;
}
