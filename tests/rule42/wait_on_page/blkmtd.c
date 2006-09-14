/* 
 * $Id$
 *
 * blkmtd.c - use a block device as a fake MTD
 *
 * Author: Simon Evans <spse@secret.org.uk>
 *
 * Copyright (C) 2001 Simon Evans
 * 
 * Licence: GPL
 *
 * How it works:
 *       The driver uses raw/io to read/write the device and the page
 *       cache to cache access. Writes update the page cache with the
 *       new data but make a copy of the new page(s) and then a kernel
 *       thread writes pages out to the device in the background. This
 *       ensures that writes are order even if a page is updated twice.
 *       Also, since pages in the page cache are never marked as dirty,
 *       we dont have to worry about writepage() being called on some 
 *       random page which may not be in the write order.
 * 
 *       Erases are handled like writes, so the callback is called after
 *       the page cache has been updated. Sync()ing will wait until it is 
 *       all done.
 *
 *       It can be loaded Read-Only to prevent erases and writes to the 
 *       medium.
 *
 * Todo:
 *       Make the write queue size dynamic so this it is not too big on
 *       small memory systems and too small on large memory systems.
 * 
 *       Page cache usage may still be a bit wrong. Check we are doing
 *       everything properly.
 * 
 *       Somehow allow writes to dirty the page cache so we dont use too
 *       much memory making copies of outgoing pages. Need to handle case
 *       where page x is written to, then page y, then page x again before
 *       any of them have been committed to disk.
 * 
 *       Reading should read multiple pages at once rather than using 
 *       readpage() for each one. This is easy and will be fixed asap.
 */


#include <linux/config.h>
#include <linux/module.h>

#include <linux/fs.h>
#include <linux/blkdev.h>
#include <linux/pagemap.h>
#include <linux/iobuf.h>
#include <linux/slab.h>
#include <linux/pagemap.h>
#include <linux/mtd/compatmac.h>
#include <linux/mtd/mtd.h>

#ifdef CONFIG_MTD_DEBUG
#ifdef CONFIG_PROC_FS
#  include <linux/proc_fs.h>
#  define BLKMTD_PROC_DEBUG
   static struct proc_dir_entry *blkmtd_proc;
#endif
#endif


/* Default erase size in K, always make it a multiple of PAGE_SIZE */
#define CONFIG_MTD_BLKDEV_ERASESIZE 128
#define VERSION "1.7"

/* Info for the block device */
typedef struct mtd_raw_dev_data_s {
  struct block_device *binding;
  int sector_size, sector_bits;
  int partial_last_page;   // 0 if device ends on page boundary, else page no of last page
  int last_page_sectors;   // Number of sectors in last page if partial_last_page != 0
  size_t totalsize;
  int readonly;
  struct address_space as;
  struct mtd_info mtd_info;
} mtd_raw_dev_data_t;

/* Info for each queue item in the write queue */
typedef struct mtdblkdev_write_queue_s {
  mtd_raw_dev_data_t *rawdevice;
  struct page **pages;
  int pagenr;
  int pagecnt;
  int iserase;
} mtdblkdev_write_queue_t;


/* Our erase page - always remains locked. */
static struct page *erase_page;

/* Static info about the MTD, used in cleanup_module */
static mtd_raw_dev_data_t *mtd_rawdevice;

/* Write queue fixed size */
#define WRITE_QUEUE_SZ 512

/* Storage for the write queue */
static mtdblkdev_write_queue_t *write_queue;
static int write_queue_sz = WRITE_QUEUE_SZ;
static int volatile write_queue_head;
static int volatile write_queue_tail;
static int volatile write_queue_cnt;
static spinlock_t mbd_writeq_lock = SPIN_LOCK_UNLOCKED;

/* Tell the write thread to finish */
static volatile int write_task_finish;

/* ipc with the write thread */
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,3,0)
static DECLARE_MUTEX_LOCKED(thread_sem);
static DECLARE_WAIT_QUEUE_HEAD(thr_wq);
static DECLARE_WAIT_QUEUE_HEAD(mtbd_sync_wq);
#else
static struct semaphore thread_sem = MUTEX_LOCKED;
DECLARE_WAIT_QUEUE_HEAD(thr_wq);
DECLARE_WAIT_QUEUE_HEAD(mtbd_sync_wq);
#endif


/* Module parameters passed by insmod/modprobe */
char *device;    /* the block device to use */
int erasesz;     /* optional default erase size */
int ro;          /* optional read only flag */
int bs;          /* optionally force the block size (avoid using) */
int count;       /* optionally force the block count (avoid using) */
int wqs;         /* optionally set the write queue size */


#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,2,0)
MODULE_LICENSE("GPL");
MODULE_AUTHOR("Simon Evans <spse@secret.org.uk>");
MODULE_DESCRIPTION("Emulate an MTD using a block device");
MODULE_PARM(device, "s");
MODULE_PARM_DESC(device, "block device to use");
MODULE_PARM(erasesz, "i");
MODULE_PARM_DESC(erasesz, "optional erase size to use in KB. eg 4=4K.");
MODULE_PARM(ro, "i");
MODULE_PARM_DESC(ro, "1=Read only, writes and erases cause errors");
MODULE_PARM(bs, "i");
MODULE_PARM_DESC(bs, "force the block size in bytes");
MODULE_PARM(count, "i");
MODULE_PARM_DESC(count, "force the block count");
MODULE_PARM(wqs, "i");
#endif


/* Page cache stuff */

/* writepage() - should never be called - catch it anyway */
static int blkmtd_writepage(struct page *page)
{
  printk("blkmtd: writepage called!!!\n");
  return -EIO;
}


/* readpage() - reads one page from the block device */                 
static int blkmtd_readpage(mtd_raw_dev_data_t *rawdevice, struct page *page)
{  
  int err;
  int sectornr, sectors, i;
  struct kiobuf *iobuf;
  unsigned long *blocks;

  if(!rawdevice) {
    printk("blkmtd: readpage: PANIC file->private_data == NULL\n");
    return -EIO;
  }

  DEBUG(2, "blkmtd: readpage called, dev = `%s' page = %p index = %ld\n",
	bdevname(rawdevice->binding), page, page->index);

  if(PageUptodate(page)) {
    DEBUG(2, "blkmtd: readpage page %ld is already upto date\n", page->index);
    unlock_page(page);
    return 0;
  }

  ClearPageUptodate(page);
  ClearPageError(page);

  /* see if page is in the outgoing write queue */
  spin_lock(&mbd_writeq_lock);
  if(write_queue_cnt) {
    int i = write_queue_tail;
    while(i != write_queue_head) {
      mtdblkdev_write_queue_t *item = &write_queue[i];
      if(page->index >= item->pagenr && page->index < item->pagenr+item->pagecnt) {
	/* yes it is */
	int index = page->index - item->pagenr;
		
	DEBUG(2, "blkmtd: readpage: found page %ld in outgoing write queue\n",
	      page->index);
	if(item->iserase) {
	  memset(page_address(page), 0xff, PAGE_SIZE);
	} else {
	  memcpy(page_address(page), page_address(item->pages[index]), PAGE_SIZE);
	}
	SetPageUptodate(page);
	flush_dcache_page(page);
	unlock_page(page);
	spin_unlock(&mbd_writeq_lock);
	return 0;
      }
      i++;
      i %= write_queue_sz;
    }
  }
  spin_unlock(&mbd_writeq_lock);


  DEBUG(3, "blkmtd: readpage: getting kiovec\n");
  err = alloc_kiovec(1, &iobuf);
  if (err) {
    printk("blkmtd: cant allocate kiobuf\n");
    SetPageError(page);
    return err;
  }

  /* Pre 2.4.4 doesnt have space for the block list in the kiobuf */ 
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,4,4)
  blocks = kmalloc(KIO_MAX_SECTORS * sizeof(unsigned long));
  if(blocks == NULL) {
    printk("blkmtd: cant allocate iobuf blocks\n");
    free_kiovec(1, &iobuf);
    SetPageError(page);
    return -ENOMEM;
  }
#else 
  blocks = iobuf->blocks;
#endif

  iobuf->offset = 0;
  iobuf->nr_pages = 1;
  iobuf->length = PAGE_SIZE;
  iobuf->locked = 1;
  iobuf->maplist[0] = page;
  sectornr = page->index << (PAGE_SHIFT - rawdevice->sector_bits);
  sectors = 1 << (PAGE_SHIFT - rawdevice->sector_bits);
  if(rawdevice->partial_last_page && page->index == rawdevice->partial_last_page) {
    DEBUG(3, "blkmtd: handling partial last page\n");
    sectors = rawdevice->last_page_sectors;
  }
  DEBUG(3, "blkmtd: readpage: sectornr = %d sectors = %d\n", sectornr, sectors);
  for(i = 0; i < sectors; i++) {
    blocks[i] = sectornr++;
  }
  /* If only a partial page read in, clear the rest of the page */
  if(rawdevice->partial_last_page && page->index == rawdevice->partial_last_page) {
    int offset = rawdevice->last_page_sectors << rawdevice->sector_bits;
    int count = PAGE_SIZE-offset;
    DEBUG(3, "blkmtd: clear last partial page: offset = %d count = %d\n", offset, count);
    memset(page_address(page)+offset, 0, count);
    sectors = rawdevice->last_page_sectors;
  }


  DEBUG(3, "bklmtd: readpage: starting brw_kiovec\n");
  err = brw_kiovec(READ, 1, &iobuf, rawdevice->binding, blocks, rawdevice->sector_size);
  DEBUG(3, "blkmtd: readpage: finished, err = %d\n", err);
  iobuf->locked = 0;
  free_kiovec(1, &iobuf);

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,4,4)
  kfree(blocks);
#endif

  if(err != PAGE_SIZE) {
    printk("blkmtd: readpage: error reading page %ld\n", page->index);
    memset(page_address(page), 0, PAGE_SIZE);
    SetPageError(page);
    err = -EIO;
  } else {
    DEBUG(3, "blkmtd: readpage: setting page upto date\n");
    SetPageUptodate(page);
    err = 0;
  }
  flush_dcache_page(page);
  unlock_page(page);
  DEBUG(2, "blkmtd: readpage: finished, err = %d\n", err);
  return 0;
}

                    
static struct address_space_operations blkmtd_aops = {
  writepage:     blkmtd_writepage,
  readpage:      NULL,
}; 


/* This is the kernel thread that empties the write queue to disk */
static int write_queue_task(void *data)
{
  int err;
  struct task_struct *tsk = current;
  struct kiobuf *iobuf;
  unsigned long *blocks;

  DECLARE_WAITQUEUE(wait, tsk);
  DEBUG(1, "blkmtd: writetask: starting (pid = %d)\n", tsk->pid);
  daemonize();
  strcpy(tsk->comm, "blkmtdd");
  tsk->tty = NULL;
  spin_lock_irq(&tsk->sigmask_lock);
  sigfillset(&tsk->blocked);
  recalc_sigpending();
  spin_unlock_irq(&tsk->sigmask_lock);

  if(alloc_kiovec(1, &iobuf)) {
    printk("blkmtd: write_queue_task cant allocate kiobuf\n");
    return 0;
  }

  /* Pre 2.4.4 doesnt have space for the block list in the kiobuf */ 
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,4,4)
  blocks = kmalloc(KIO_MAX_SECTORS * sizeof(unsigned long));
  if(blocks == NULL) {
    printk("blkmtd: write_queue_task cant allocate iobuf blocks\n");
    free_kiovec(1, &iobuf);
    return 0;
  }
#else 
  blocks = iobuf->blocks;
#endif

  DEBUG(2, "blkmtd: writetask: entering main loop\n");
  add_wait_queue(&thr_wq, &wait);

  while(1) {
    spin_lock(&mbd_writeq_lock);

    if(!write_queue_cnt) {
      /* If nothing in the queue, wake up anyone wanting to know when there
	 is space in the queue then sleep for 2*HZ */
      spin_unlock(&mbd_writeq_lock);
      DEBUG(4, "blkmtd: writetask: queue empty\n");
      if(waitqueue_active(&mtbd_sync_wq))
	 wake_up(&mtbd_sync_wq);
      interruptible_sleep_on_timeout(&thr_wq, 2*HZ);
      DEBUG(4, "blkmtd: writetask: woken up\n");
      if(write_task_finish)
	break;
    } else {
      /* we have stuff to write */
      mtdblkdev_write_queue_t *item = &write_queue[write_queue_tail];
      struct page **pages = item->pages;

      int i;
      int sectornr = item->pagenr << (PAGE_SHIFT - item->rawdevice->sector_bits);
      int sectorcnt = item->pagecnt << (PAGE_SHIFT - item->rawdevice->sector_bits);
      int max_sectors = KIO_MAX_SECTORS >> (item->rawdevice->sector_bits - 9);

      /* If we are writing to the last page on the device and it doesnt end
       * on a page boundary, subtract the number of sectors that dont exist.
       */
      if(item->rawdevice->partial_last_page && 
	 (item->pagenr + item->pagecnt -1) == item->rawdevice->partial_last_page) {
	sectorcnt -= (1 << (PAGE_SHIFT - item->rawdevice->sector_bits));
	sectorcnt += item->rawdevice->last_page_sectors;
      }

      DEBUG(3, "blkmtd: writetask: got %d queue items\n", write_queue_cnt);
      set_current_state(TASK_RUNNING);
      spin_unlock(&mbd_writeq_lock);

      DEBUG(2, "blkmtd: writetask: writing pagenr = %d pagecnt = %d sectornr = %d sectorcnt = %d\n", 
	    item->pagenr, item->pagecnt, sectornr, sectorcnt);

      iobuf->offset = 0;
      iobuf->locked = 1;

      /* Loop through all the pages to be written in the queue item, remembering
	 we can only write KIO_MAX_SECTORS at a time */
	 
      while(sectorcnt) {
	int cursectors = (sectorcnt < max_sectors) ? sectorcnt : max_sectors;
	int cpagecnt = (cursectors << item->rawdevice->sector_bits) + PAGE_SIZE-1;
	cpagecnt >>= PAGE_SHIFT;
	
	for(i = 0; i < cpagecnt; i++) {
	  if(item->iserase) {
	    iobuf->maplist[i] = erase_page;
	  } else {
	    iobuf->maplist[i] = *(pages++);
	  }
	}
	
	for(i = 0; i < cursectors; i++) {
	  blocks[i] = sectornr++;
	}
	
	iobuf->nr_pages = cpagecnt;
	iobuf->length = cursectors << item->rawdevice->sector_bits;
	DEBUG(3, "blkmtd: write_task: about to kiovec\n");
	err = brw_kiovec(WRITE, 1, &iobuf, item->rawdevice->binding, blocks, item->rawdevice->sector_size);
	DEBUG(3, "bklmtd: write_task: done, err = %d\n", err);
	if(err != (cursectors << item->rawdevice->sector_bits)) {
	  /* if an error occured - set this to exit the loop */
	  sectorcnt = 0;
	} else {
	  sectorcnt -= cursectors;
	}
      }

      /* free up the pages used in the write and list of pages used in the write
	 queue item */
      iobuf->locked = 0;
      spin_lock(&mbd_writeq_lock);
      write_queue_cnt--;
      write_queue_tail++;
      write_queue_tail %= write_queue_sz;
      if(!item->iserase) {
	for(i = 0 ; i < item->pagecnt; i++) {
	  unlock_page(item->pages[i]);
	  __free_pages(item->pages[i], 0);
	}
	kfree(item->pages);
      }
      item->pages = NULL;
      spin_unlock(&mbd_writeq_lock);
      /* Tell others there is some space in the write queue */
      if(waitqueue_active(&mtbd_sync_wq))
	wake_up(&mtbd_sync_wq);
    }
  }
  remove_wait_queue(&thr_wq, &wait);
  DEBUG(1, "blkmtd: writetask: exiting\n");
  free_kiovec(1, &iobuf);

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,4,4)
  kfree(blocks);
#endif

  /* Tell people we have exitd */
  up(&thread_sem);
  return 0;
}


/* Add a range of pages into the outgoing write queue, making copies of them */
static int queue_page_write(mtd_raw_dev_data_t *rawdevice, struct page **pages,
			    int pagenr, int pagecnt, int iserase)
{
  struct page *outpage;
  struct page **new_pages = NULL;
  mtdblkdev_write_queue_t *item;
  int i;
  DECLARE_WAITQUEUE(wait, current);
  DEBUG(2, "blkmtd: queue_page_write: adding pagenr = %d pagecnt = %d\n", pagenr, pagecnt);

  if(!pagecnt)
    return 0;

  if(pages == NULL && !iserase)
    return -EINVAL;

  /* create a array for the list of pages */
  if(!iserase) {
    new_pages = kmalloc(pagecnt * sizeof(struct page *), GFP_KERNEL);
    if(new_pages == NULL)
      return -ENOMEM;

    /* make copies of the pages in the page cache */
    for(i = 0; i < pagecnt; i++) {
      outpage = alloc_pages(GFP_KERNEL, 0);
      if(!outpage) {
	while(i--) {
	  unlock_page(new_pages[i]);
	  __free_pages(new_pages[i], 0);
	}
	kfree(new_pages);
	return -ENOMEM;
      }
      lock_page(outpage);
      memcpy(page_address(outpage), page_address(pages[i]), PAGE_SIZE);
      new_pages[i] = outpage;
    }
  }

  /* wait until there is some space in the write queue */
 test_lock:
  spin_lock(&mbd_writeq_lock);
  if(write_queue_cnt == write_queue_sz) {
    spin_unlock(&mbd_writeq_lock);
    DEBUG(3, "blkmtd: queue_page: Queue full\n");
    current->state = TASK_UNINTERRUPTIBLE;
    add_wait_queue(&mtbd_sync_wq, &wait);
    wake_up_interruptible(&thr_wq);
    schedule();
    current->state = TASK_RUNNING;
    remove_wait_queue(&mtbd_sync_wq, &wait);
    DEBUG(3, "blkmtd: queue_page_write: Queue has %d items in it\n", write_queue_cnt);
    goto test_lock;
  }

  DEBUG(3, "blkmtd: queue_page_write: qhead: %d qtail: %d qcnt: %d\n", 
	write_queue_head, write_queue_tail, write_queue_cnt);

  /* fix up the queue item */
  item = &write_queue[write_queue_head];
  item->pages = new_pages;
  item->pagenr = pagenr;
  item->pagecnt = pagecnt;
  item->rawdevice = rawdevice;
  item->iserase = iserase;

  write_queue_head++;
  write_queue_head %= write_queue_sz;
  write_queue_cnt++;
  DEBUG(3, "blkmtd: queue_page_write: qhead: %d qtail: %d qcnt: %d\n", 
	write_queue_head, write_queue_tail, write_queue_cnt);
  spin_unlock(&mbd_writeq_lock);
  DEBUG(2, "blkmtd: queue_page_write: finished\n");
  return 0;
}


/* erase a specified part of the device */
static int blkmtd_erase(struct mtd_info *mtd, struct erase_info *instr)
{
  mtd_raw_dev_data_t *rawdevice = mtd->priv;
  struct mtd_erase_region_info *einfo = mtd->eraseregions;
  int numregions = mtd->numeraseregions;
  size_t from;
  u_long len;
  int err = 0;

  /* check readonly */
  if(rawdevice->readonly) {
    printk("blkmtd: error: trying to erase readonly device %s\n", device);
    instr->state = MTD_ERASE_FAILED;
    goto erase_callback;
  }

  instr->state = MTD_ERASING;
  from = instr->addr;
  len = instr->len;

  /* check erase region has valid start and length */
  DEBUG(2, "blkmtd: erase: dev = `%s' from = 0x%x len = 0x%lx\n",
	bdevname(rawdevice->binding), from, len);
  while(numregions) {
    DEBUG(3, "blkmtd: checking erase region = 0x%08X size = 0x%X num = 0x%x\n",
	  einfo->offset, einfo->erasesize, einfo->numblocks);
    if(from >= einfo->offset && from < einfo->offset + (einfo->erasesize * einfo->numblocks)) {
      if(len == einfo->erasesize && ( (from - einfo->offset) % einfo->erasesize == 0))
	break;
    }
    numregions--;
    einfo++;
  }

  if(!numregions) {
    /* Not a valid erase block */
    printk("blkmtd: erase: invalid erase request 0x%lX @ 0x%08X\n", len, from);
    instr->state = MTD_ERASE_FAILED;
    err = -EIO;
  }
  
  if(instr->state != MTD_ERASE_FAILED) {
    /* start the erase */
    int pagenr, pagecnt;
    struct page *page, **pages;
    int i = 0;

    /* Handle the last page of the device not being whole */
    if(len < PAGE_SIZE)
      len = PAGE_SIZE;

    pagenr = from >> PAGE_SHIFT;
    pagecnt = len >> PAGE_SHIFT;
    DEBUG(3, "blkmtd: erase: pagenr = %d pagecnt = %d\n", pagenr, pagecnt);

    pages = kmalloc(pagecnt * sizeof(struct page *), GFP_KERNEL);
    if(pages == NULL) {
      err = -ENOMEM;
      instr->state = MTD_ERASE_FAILED;
      goto erase_out;
    }


    while(pagecnt) {
      /* get the page via the page cache */
      DEBUG(3, "blkmtd: erase: doing grab_cache_page() for page %d\n", pagenr);
      page = grab_cache_page(&rawdevice->as, pagenr);
      if(!page) {
	DEBUG(3, "blkmtd: erase: grab_cache_page() failed for page %d\n", pagenr);
	kfree(pages);
	err = -EIO;
	instr->state = MTD_ERASE_FAILED;
	goto erase_out;
      }
      memset(page_address(page), 0xff, PAGE_SIZE);
      pages[i] = page;
      pagecnt--;
      pagenr++;
      i++;
    }
    DEBUG(3, "blkmtd: erase: queuing page write\n");
    err = queue_page_write(rawdevice, NULL, from >> PAGE_SHIFT, len >> PAGE_SHIFT, 1);
    pagecnt = len >> PAGE_SHIFT;
    if(!err) {
      while(pagecnt--) {
	SetPageUptodate(pages[pagecnt]);
	unlock_page(pages[pagecnt]);
	page_cache_release(pages[pagecnt]);
	flush_dcache_page(pages[pagecnt]);
      }
      kfree(pages);
      instr->state = MTD_ERASE_DONE;
    } else {
      while(pagecnt--) {
	SetPageError(pages[pagecnt]);
	page_cache_release(pages[pagecnt]);
      }
      kfree(pages);
      instr->state = MTD_ERASE_FAILED;
    }
  }
 erase_out:
  DEBUG(3, "blkmtd: erase: checking callback\n");
 erase_callback:
  if (instr->callback) {
    (*(instr->callback))(instr);
  }
  DEBUG(2, "blkmtd: erase: finished (err = %d)\n", err);
  return err;
}


/* read a range of the data via the page cache */
static int blkmtd_read(struct mtd_info *mtd, loff_t from, size_t len,
	     size_t *retlen, u_char *buf)
{
  mtd_raw_dev_data_t *rawdevice = mtd->priv;
  int err = 0;
  int offset;
  int pagenr, pages;

  *retlen = 0;

  DEBUG(2, "blkmtd: read: dev = `%s' from = %ld len = %d buf = %p\n",
	bdevname(rawdevice->binding), (long int)from, len, buf);

  pagenr = from >> PAGE_SHIFT;
  offset = from - (pagenr << PAGE_SHIFT);
  
  pages = (offset+len+PAGE_SIZE-1) >> PAGE_SHIFT;
  DEBUG(3, "blkmtd: read: pagenr = %d offset = %d, pages = %d\n", pagenr, offset, pages);

  /* just loop through each page, getting it via readpage() - slow but easy */
  while(pages) {
    struct page *page;
    int cpylen;
    DEBUG(3, "blkmtd: read: looking for page: %d\n", pagenr);
    page = read_cache_page(&rawdevice->as, pagenr, (filler_t *)blkmtd_readpage, rawdevice);
    if(IS_ERR(page)) {
      return PTR_ERR(page);
    }
    wait_on_page(page);
    if(!PageUptodate(page)) {
      /* error reading page */
      printk("blkmtd: read: page not uptodate\n");
      page_cache_release(page);
      return -EIO;
    }

    cpylen = (PAGE_SIZE > len) ? len : PAGE_SIZE;
    if(offset+cpylen > PAGE_SIZE)
      cpylen = PAGE_SIZE-offset;
    
    memcpy(buf + *retlen, page_address(page) + offset, cpylen);
    offset = 0;
    len -= cpylen;
    *retlen += cpylen;
    pagenr++;
    pages--;
    page_cache_release(page);
  }
  
  DEBUG(2, "blkmtd: end read: retlen = %d, err = %d\n", *retlen, err);
  return err;
}

    
/* write a range of the data via the page cache.
 *
 * Basic operation. break the write into three parts. 
 *
 * 1. From a page unaligned start up until the next page boundary
 * 2. Page sized, page aligned blocks
 * 3. From end of last aligned block to end of range
 *
 * 1,3 are read via the page cache and readpage() since these are partial
 * pages, 2 we just grab pages from the page cache, not caring if they are
 * already in memory or not since they will be completly overwritten.
 *
 */
 
static int blkmtd_write(struct mtd_info *mtd, loff_t to, size_t len,
	      size_t *retlen, const u_char *buf)
{
  mtd_raw_dev_data_t *rawdevice = mtd->priv;
  int err = 0;
  int offset;
  int pagenr;
  size_t len1 = 0, len2 = 0, len3 = 0;
  struct page **pages;
  int pagecnt = 0;

  *retlen = 0;
  DEBUG(2, "blkmtd: write: dev = `%s' to = %ld len = %d buf = %p\n",
	bdevname(rawdevice->binding), (long int)to, len, buf);

  /* handle readonly and out of range numbers */

  if(rawdevice->readonly) {
    printk("blkmtd: error: trying to write to a readonly device %s\n", device);
    return -EROFS;
  }

  if(to >= rawdevice->totalsize) {
    return -ENOSPC;
  }

  if(to + len > rawdevice->totalsize) {
    len = (rawdevice->totalsize - to);
  }


  pagenr = to >> PAGE_SHIFT;
  offset = to - (pagenr << PAGE_SHIFT);

  /* see if we have to do a partial write at the start */
  if(offset) {
    if((offset + len) > PAGE_SIZE) {
      len1 = PAGE_SIZE - offset;
      len -= len1;
    } else {
      len1 = len;
      len = 0;
    }
  }

  /* calculate the length of the other two regions */
  len3 = len & ~PAGE_MASK;
  len -= len3;
  len2 = len;


  if(len1)
    pagecnt++;
  if(len2)
    pagecnt += len2 >> PAGE_SHIFT;
  if(len3)
    pagecnt++;

  DEBUG(3, "blkmtd: write: len1 = %d len2 = %d len3 = %d pagecnt = %d\n", len1, len2, len3, pagecnt);
  
  /* get space for list of pages */
  pages = kmalloc(pagecnt * sizeof(struct page *), GFP_KERNEL);
  if(pages == NULL) {
    return -ENOMEM;
  }
  pagecnt = 0;

  if(len1) {
    /* do partial start region */
    struct page *page;
    
    DEBUG(3, "blkmtd: write: doing partial start, page = %d len = %d offset = %d\n", pagenr, len1, offset);
    page = read_cache_page(&rawdevice->as, pagenr, (filler_t *)blkmtd_readpage, rawdevice);

    if(IS_ERR(page)) {
      kfree(pages);
      return PTR_ERR(page);
    }
    memcpy(page_address(page)+offset, buf, len1);
    pages[pagecnt++] = page;
    buf += len1;
    *retlen = len1;
    err = 0;
    pagenr++;
  }

  /* Now do the main loop to a page aligned, n page sized output */
  if(len2) {
    int pagesc = len2 >> PAGE_SHIFT;
    DEBUG(3, "blkmtd: write: whole pages start = %d, count = %d\n", pagenr, pagesc);
    while(pagesc) {
      struct page *page;

      /* see if page is in the page cache */
      DEBUG(3, "blkmtd: write: grabbing page %d from page cache\n", pagenr);
      page = grab_cache_page(&rawdevice->as, pagenr);
      DEBUG(3, "blkmtd: write: got page %d from page cache\n", pagenr);
      if(!page) {
	printk("blkmtd: write: cant grab cache page %d\n", pagenr);
	err = -EIO;
	goto write_err;
      }
      memcpy(page_address(page), buf, PAGE_SIZE);
      pages[pagecnt++] = page;
      unlock_page(page);
      SetPageUptodate(page);
      pagenr++;
      pagesc--;
      buf += PAGE_SIZE;
      *retlen += PAGE_SIZE;
    }
  }


  if(len3) {
    /* do the third region */
    struct page *page;
    DEBUG(3, "blkmtd: write: doing partial end, page = %d len = %d\n", pagenr, len3);
    page = read_cache_page(&rawdevice->as, pagenr, (filler_t *)blkmtd_readpage, rawdevice);
    if(IS_ERR(page)) {
      err = PTR_ERR(page);
      goto write_err;
    }
    memcpy(page_address(page), buf, len3);
    DEBUG(3, "blkmtd: write: writing out partial end\n");
    pages[pagecnt++] = page;
    *retlen += len3;
    err = 0;
  }
  DEBUG(2, "blkmtd: write: end, retlen = %d, err = %d\n", *retlen, err);
  /* submit it to the write task */
  err = queue_page_write(rawdevice, pages, to >> PAGE_SHIFT, pagecnt, 0);
  if(!err) {
    while(pagecnt--) {
      SetPageUptodate(pages[pagecnt]);
      flush_dcache_page(pages[pagecnt]);
      page_cache_release(pages[pagecnt]);
    }
    kfree(pages);
    return 0;
  }

 write_err:
  while(--pagecnt) {
    SetPageError(pages[pagecnt]);
    page_cache_release(pages[pagecnt]);
  }
  kfree(pages);
  return err;
}


/* sync the device - wait until the write queue is empty */
static void blkmtd_sync(struct mtd_info *mtd)
{
  DECLARE_WAITQUEUE(wait, current);
  mtd_raw_dev_data_t *rawdevice = mtd->priv;
  if(rawdevice->readonly)
    return;

  DEBUG(2, "blkmtd: sync: called\n");

 stuff_inq:
  spin_lock(&mbd_writeq_lock);
  if(write_queue_cnt) {
    spin_unlock(&mbd_writeq_lock);
    current->state = TASK_UNINTERRUPTIBLE;
    add_wait_queue(&mtbd_sync_wq, &wait);
    DEBUG(3, "blkmtd: sync: waking up task\n");
    wake_up_interruptible(&thr_wq);
    schedule();
    current->state = TASK_RUNNING;
    remove_wait_queue(&mtbd_sync_wq, &wait);
    DEBUG(3, "blkmtd: sync: waking up after write task\n");
    goto stuff_inq;
  }
  spin_unlock(&mbd_writeq_lock);

  DEBUG(2, "blkmtd: sync: finished\n");
}


#ifdef BLKMTD_PROC_DEBUG
/* procfs stuff */
static int blkmtd_proc_read(char *page, char **start, off_t off, int count, int *eof, void *data)
{
  int clean = 0, dirty = 0, locked = 0;
  struct list_head *temp;
  int i, len, pages = 0, cnt;
  MOD_INC_USE_COUNT;
  spin_lock(&mbd_writeq_lock);
  cnt = write_queue_cnt;
  i = write_queue_tail;
  while(cnt) {
    if(!write_queue[i].iserase)
      pages += write_queue[i].pagecnt;
    i++;
    i %= write_queue_sz;
    cnt--;
  }

  /* Count the size of the page lists */
  list_for_each(temp, &mtd_rawdevice->as.clean_pages) {
    clean++;
  }
  list_for_each(temp, &mtd_rawdevice->as.dirty_pages) {
    dirty++;
  }
  list_for_each(temp, &mtd_rawdevice->as.locked_pages) {
    locked++;
  }

  len = sprintf(page, "Write queue head: %d\nWrite queue tail: %d\n"
		"Write queue count: %d\nPages in queue: %d (%dK)\n"
		"Clean Pages: %d\nDirty Pages: %d\nLocked Pages: %d\n"
		"nrpages: %ld\n",
		write_queue_head, write_queue_tail, write_queue_cnt,
		pages, pages << (PAGE_SHIFT-10), clean, dirty, locked,
		mtd_rawdevice->as.nrpages);
  if(len <= count)
    *eof = 1;
  spin_unlock(&mbd_writeq_lock);
  MOD_DEC_USE_COUNT;
  return len;
}
#endif


/* Cleanup and exit - sync the device and kill of the kernel thread */
static void __exit cleanup_blkmtd(void)
{
#ifdef BLKMTD_PROC_DEBUG
  if(blkmtd_proc) {
    remove_proc_entry("blkmtd_debug", NULL);
  }
#endif

  if (mtd_rawdevice) {
    /* sync the device */
    if (!mtd_rawdevice->readonly) {
      blkmtd_sync(&mtd_rawdevice->mtd_info);
      write_task_finish = 1;
      wake_up_interruptible(&thr_wq);
      down(&thread_sem);
    }
    del_mtd_device(&mtd_rawdevice->mtd_info);
    if(mtd_rawdevice->binding != NULL)
      blkdev_put(mtd_rawdevice->binding, BDEV_RAW);

    if(mtd_rawdevice->mtd_info.eraseregions)
      kfree(mtd_rawdevice->mtd_info.eraseregions);
    if(mtd_rawdevice->mtd_info.name)
      kfree(mtd_rawdevice->mtd_info.name);

    kfree(mtd_rawdevice);
  }
  if(write_queue)
    kfree(write_queue);

  if(erase_page) {
    unlock_page(erase_page);
    __free_pages(erase_page, 0);
  }
  printk("blkmtd: unloaded for %s\n", device);
}

extern struct module __this_module;

#ifndef MODULE

/* Handle kernel boot params */


static int __init param_blkmtd_device(char *str)
{
  device = str;
  return 1;
}


static int __init param_blkmtd_erasesz(char *str)
{
  erasesz = simple_strtol(str, NULL, 0);
  return 1;
}


static int __init param_blkmtd_ro(char *str)
{
  ro = simple_strtol(str, NULL, 0);
  return 1;
}


static int __init param_blkmtd_bs(char *str)
{
  bs = simple_strtol(str, NULL, 0);
  return 1;
}


static int __init param_blkmtd_count(char *str)
{
  count = simple_strtol(str, NULL, 0);
  return 1;
}

__setup("blkmtd_device=", param_blkmtd_device);
__setup("blkmtd_erasesz=", param_blkmtd_erasesz);
__setup("blkmtd_ro=", param_blkmtd_ro);
__setup("blkmtd_bs=", param_blkmtd_bs);
__setup("blkmtd_count=", param_blkmtd_count);

#endif


/* for a given size and initial erase size, calculate the number and size of each
   erase region */
static int __init calc_erase_regions(struct mtd_erase_region_info *info, size_t erase_size, size_t total_size)
{
  int count = 0;
  int offset = 0;
  int regions = 0;

   while(total_size) {
     count = total_size / erase_size;
     if(count) {
       total_size = total_size % erase_size;
       if(info) {
	 info->offset = offset;
	 info->erasesize = erase_size;
	 info->numblocks = count;
	 info++;
       }
       offset += (count * erase_size);
       regions++;
     }
     while(erase_size > total_size)
       erase_size >>= 1;
   }
   return regions;
}


extern kdev_t name_to_kdev_t(char *line) __init;

/* Startup */
static int __init init_blkmtd(void)
{
#ifdef MODULE
  struct file *file = NULL;
  struct inode *inode;
#endif

  int maj, min;
  int i, blocksize, blocksize_bits;
  loff_t size;
  int readonly = 0;
  int erase_size = CONFIG_MTD_BLKDEV_ERASESIZE;
  kdev_t rdev;
  int err;
  int mode;
  int regions;

  /* Check args */
  if(device == 0) {
    printk("blkmtd: error, missing `device' name\n");
    return -EINVAL;
  }

  if(ro)
    readonly = 1;

  if(erasesz)
    erase_size = erasesz;

  if(wqs) {
    if(wqs < 16) 
      wqs = 16;
    if(wqs > 4*WRITE_QUEUE_SZ)
      wqs = 4*WRITE_QUEUE_SZ;
    write_queue_sz = wqs;
  }

  DEBUG(1, "blkmtd: device = `%s' erase size = %dK readonly = %s queue size = %d\n",
	device, erase_size, readonly ? "yes" : "no", write_queue_sz);
  /* Get a handle on the device */
  mode = (readonly) ? O_RDONLY : O_RDWR;

#ifdef MODULE

  file = filp_open(device, mode, 0);
  if(IS_ERR(file)) {
    printk("blkmtd: error, cant open device %s\n", device);
    DEBUG(2, "blkmtd: filp_open returned %ld\n", PTR_ERR(file));
    return 1;
  }
  
  /* determine is this is a block device and if so get its major and minor
     numbers */
  inode = file->f_dentry->d_inode;
  if(!S_ISBLK(inode->i_mode)) {
    printk("blkmtd: %s not a block device\n", device);
    filp_close(file, NULL);
    return 1;
  }
  rdev = inode->i_rdev;
  filp_close(file, NULL);
#else
  rdev = name_to_kdev_t(device);
#endif

  maj = major(rdev);
  min = minor(rdev);
  DEBUG(1, "blkmtd: found a block device major = %d, minor = %d\n", maj, min);

  if(kdev_none(rdev)) {
    printk("blkmtd: bad block device: `%s'\n", device);
    return 1;
  }

  if(maj == MTD_BLOCK_MAJOR) {
    printk("blkmtd: attempting to use an MTD device as a block device\n");
    return 1;
  }

  DEBUG(1, "blkmtd: devname = %s\n", __bdevname(rdev));
  blocksize = BLOCK_SIZE;

  blocksize = bs ? bs : block_size(rdev);
  i = blocksize;
  blocksize_bits = 0;
  while(i != 1) {
    blocksize_bits++;
    i >>= 1;
  }

  size = (count ? count*blocksize : blkdev_size_in_bytes(rdev));

  DEBUG(1, "blkmtd: size = %ld\n", (long int)size);

  if(size == 0) {
    printk("blkmtd: cant determine size\n");
    return 1;
  }

  mtd_rawdevice = (mtd_raw_dev_data_t *)kmalloc(sizeof(mtd_raw_dev_data_t), GFP_KERNEL);
  if(mtd_rawdevice == NULL) {
    err = -ENOMEM;
    goto init_err;
  }
  memset(mtd_rawdevice, 0, sizeof(mtd_raw_dev_data_t));
  /* get the block device */
  mtd_rawdevice->binding = bdget(kdev_t_to_nr(mk_kdev(maj, min)));
  err = blkdev_get(mtd_rawdevice->binding, mode, 0, BDEV_RAW);
  if (err) {
    goto init_err;
  }
  mtd_rawdevice->totalsize = size;
  mtd_rawdevice->sector_size = blocksize;
  mtd_rawdevice->sector_bits = blocksize_bits;
  mtd_rawdevice->readonly = readonly;

  /* See if device ends on page boundary */
  if(size % PAGE_SIZE) {
    mtd_rawdevice->partial_last_page = size >> PAGE_SHIFT;
    mtd_rawdevice->last_page_sectors = (size & (PAGE_SIZE-1)) >> blocksize_bits;
  }

  DEBUG(2, "sector_size = %d, sector_bits = %d, partial_last_page = %d last_page_sectors = %d\n", 
	mtd_rawdevice->sector_size, mtd_rawdevice->sector_bits,
	mtd_rawdevice->partial_last_page, mtd_rawdevice->last_page_sectors);

  /* Setup the MTD structure */
  /* make the name contain the block device in */
  mtd_rawdevice->mtd_info.name = kmalloc(9 + strlen(device), GFP_KERNEL);
  if(mtd_rawdevice->mtd_info.name == NULL)
    goto init_err;

  sprintf(mtd_rawdevice->mtd_info.name, "blkmtd: %s", device);
  if(readonly) {
    mtd_rawdevice->mtd_info.type = MTD_ROM;
    mtd_rawdevice->mtd_info.flags = MTD_CAP_ROM;
    mtd_rawdevice->mtd_info.erasesize = erase_size << 10;
  } else {
    mtd_rawdevice->mtd_info.type = MTD_RAM;
    mtd_rawdevice->mtd_info.flags = MTD_CAP_RAM;
    mtd_rawdevice->mtd_info.erasesize = erase_size << 10;
  }
  mtd_rawdevice->mtd_info.size = size;
  mtd_rawdevice->mtd_info.erase = blkmtd_erase;
  mtd_rawdevice->mtd_info.read = blkmtd_read;
  mtd_rawdevice->mtd_info.write = blkmtd_write;
  mtd_rawdevice->mtd_info.sync = blkmtd_sync;
  mtd_rawdevice->mtd_info.point = 0;
  mtd_rawdevice->mtd_info.unpoint = 0;

  mtd_rawdevice->mtd_info.priv = mtd_rawdevice;
  regions = calc_erase_regions(NULL, erase_size << 10, size);
  DEBUG(1, "blkmtd: init: found %d erase regions\n", regions);
  mtd_rawdevice->mtd_info.eraseregions = kmalloc(regions * sizeof(struct mtd_erase_region_info), GFP_KERNEL);
  if(mtd_rawdevice->mtd_info.eraseregions == NULL) {
    err = -ENOMEM;
    goto init_err;
  }
  mtd_rawdevice->mtd_info.numeraseregions = regions;
  calc_erase_regions(mtd_rawdevice->mtd_info.eraseregions, erase_size << 10, size);

  /* setup the page cache info */
  
  mtd_rawdevice->as.nrpages = 0;
  INIT_LIST_HEAD(&mtd_rawdevice->as.clean_pages);
  INIT_LIST_HEAD(&mtd_rawdevice->as.dirty_pages);
  INIT_LIST_HEAD(&mtd_rawdevice->as.locked_pages);
  mtd_rawdevice->as.host = NULL;
  spin_lock_init(&(mtd_rawdevice->as.i_shared_lock));

  mtd_rawdevice->as.a_ops = &blkmtd_aops;
  INIT_LIST_HEAD(&mtd_rawdevice->as.i_mmap);
  INIT_LIST_HEAD(&mtd_rawdevice->as.i_mmap_shared);
  mtd_rawdevice->as.gfp_mask = GFP_KERNEL;

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,2,0)
  mtd_rawdevice->mtd_info.module = THIS_MODULE;			
#endif
  if (add_mtd_device(&mtd_rawdevice->mtd_info)) {
    err = -EIO;
    goto init_err;
  }

  if(!mtd_rawdevice->readonly) {
    /* Allocate the write queue */
    write_queue = kmalloc(write_queue_sz * sizeof(mtdblkdev_write_queue_t), GFP_KERNEL);
    if(!write_queue) {
      err = -ENOMEM;
      goto init_err;
    }
    /* Set up the erase page */
    erase_page = alloc_pages(GFP_KERNEL, 0);
    if(erase_page == NULL) {
      err = -ENOMEM;
      goto init_err;
    }
    memset(page_address(erase_page), 0xff, PAGE_SIZE);
    lock_page(erase_page);

    init_waitqueue_head(&thr_wq);
    init_waitqueue_head(&mtbd_sync_wq);
    DEBUG(3, "blkmtd: init: kernel task @ %p\n", write_queue_task);
    DEBUG(2, "blkmtd: init: starting kernel task\n");
    kernel_thread(write_queue_task, NULL, CLONE_FS | CLONE_FILES | CLONE_SIGHAND);
    DEBUG(2, "blkmtd: init: started\n");
    printk("blkmtd loaded: version = %s using %s erase_size = %dK %s\n",
	   VERSION, device, erase_size, (readonly) ? "(read-only)" : "");
  }

#ifdef BLKMTD_PROC_DEBUG
  /* create proc entry */
  DEBUG(2, "Creating /proc/blkmtd_debug\n");
  blkmtd_proc = create_proc_read_entry("blkmtd_debug", 0444,
				       NULL, blkmtd_proc_read, NULL);
  if(blkmtd_proc == NULL) {
    printk("Cant create /proc/blkmtd_debug\n");
  } else {
    blkmtd_proc->owner = THIS_MODULE;
  }
#endif
  
  /* Everything is ok if we got here */
  return 0;
  
 init_err:

  if(mtd_rawdevice) {
    if(mtd_rawdevice->mtd_info.eraseregions)
      kfree(mtd_rawdevice->mtd_info.eraseregions);
    if(mtd_rawdevice->mtd_info.name)
      kfree(mtd_rawdevice->mtd_info.name);
    if(mtd_rawdevice->binding) 
      blkdev_put(mtd_rawdevice->binding, BDEV_RAW);
    kfree(mtd_rawdevice);
  }

  if(write_queue) {
    kfree(write_queue);
    write_queue = NULL;
  }

  if(erase_page) 
    __free_pages(erase_page, 0);
  return err;
}

module_init(init_blkmtd);
module_exit(cleanup_blkmtd);
