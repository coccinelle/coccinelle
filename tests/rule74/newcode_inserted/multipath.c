/*
 * multipath.c : Multiple Devices driver for Linux
 *
 * Copyright (C) 1999, 2000, 2001 Ingo Molnar, Red Hat
 *
 * Copyright (C) 1996, 1997, 1998 Ingo Molnar, Miguel de Icaza, Gadi Oxman
 *
 * MULTIPATH management functions.
 *
 * derived from raid1.c.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * You should have received a copy of the GNU General Public License
 * (for example /usr/src/linux/COPYING); if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <linux/module.h>
#include <linux/slab.h>
#include <linux/spinlock.h>
#include <linux/raid/multipath.h>
#include <linux/buffer_head.h>
#include <asm/atomic.h>

#define MAJOR_NR MD_MAJOR
#define MD_DRIVER
#define MD_PERSONALITY

#define MAX_WORK_PER_DISK 128

#define	NR_RESERVED_BUFS	32


static mdk_personality_t multipath_personality;
static spinlock_t retry_list_lock = SPIN_LOCK_UNLOCKED;
struct multipath_bh *multipath_retry_list = NULL, **multipath_retry_tail;


static void *mp_pool_alloc(int gfp_flags, void *data)
{
	struct multipath_bh *mpb;
	mpb = kmalloc(sizeof(*mpb), gfp_flags);
	if (mpb) 
		memset(mpb, 0, sizeof(*mpb));
	return mpb;
}

static void mp_pool_free(void *mpb, void *data)
{
	kfree(mpb);
}

static int multipath_map (mddev_t *mddev, mdk_rdev_t **rdevp)
{
	multipath_conf_t *conf = mddev_to_conf(mddev);
	int i, disks = conf->raid_disks;

	/*
	 * Later we do read balancing on the read side 
	 * now we use the first available disk.
	 */

	spin_lock_irq(&conf->device_lock);
	for (i = 0; i < disks; i++) {
		mdk_rdev_t *rdev = conf->multipaths[i].rdev;
		if (rdev && rdev->in_sync) {
			*rdevp = rdev;
			atomic_inc(&rdev->nr_pending);
			spin_unlock_irq(&conf->device_lock);
			return 0;
		}
	}
	spin_unlock_irq(&conf->device_lock);

	printk(KERN_ERR "multipath_map(): no more operational IO paths?\n");
	return (-1);
}

static void multipath_reschedule_retry (struct multipath_bh *mp_bh)
{
	unsigned long flags;
	mddev_t *mddev = mp_bh->mddev;

	spin_lock_irqsave(&retry_list_lock, flags);
	if (multipath_retry_list == NULL)
		multipath_retry_tail = &multipath_retry_list;
	*multipath_retry_tail = mp_bh;
	multipath_retry_tail = &mp_bh->next_mp;
	mp_bh->next_mp = NULL;
	spin_unlock_irqrestore(&retry_list_lock, flags);
	md_wakeup_thread(mddev->thread);
}


/*
 * multipath_end_bh_io() is called when we have finished servicing a multipathed
 * operation and are ready to return a success/failure code to the buffer
 * cache layer.
 */
static void multipath_end_bh_io (struct multipath_bh *mp_bh, int uptodate)
{
	struct bio *bio = mp_bh->master_bio;
	multipath_conf_t *conf = mddev_to_conf(mp_bh->mddev);

	bio_endio(bio, bio->bi_size, uptodate ? 0 : -EIO);
	mempool_free(mp_bh, conf->pool);
}

int multipath_end_request(struct bio *bio, unsigned int bytes_done, int error)
{
	int uptodate = test_bit(BIO_UPTODATE, &bio->bi_flags);
	struct multipath_bh * mp_bh = (struct multipath_bh *)(bio->bi_private);
	multipath_conf_t *conf = mddev_to_conf(mp_bh->mddev);
	mdk_rdev_t *rdev = conf->multipaths[mp_bh->path].rdev;

	if (bio->bi_size)
		return 1;

	if (uptodate)
		multipath_end_bh_io(mp_bh, uptodate);
	else {
		/*
		 * oops, IO error:
		 */
		char b[BDEVNAME_SIZE];
		md_error (mp_bh->mddev, rdev);
		printk(KERN_ERR "multipath: %s: rescheduling sector %llu\n", 
		       bdevname(rdev->bdev,b), 
		       (unsigned long long)bio->bi_sector);
		multipath_reschedule_retry(mp_bh);
	}
	atomic_dec(&rdev->nr_pending);
	return 0;
}

/*
 * This routine returns the disk from which the requested read should
 * be done.
 */

static int multipath_read_balance (multipath_conf_t *conf)
{
	int disk;

	for (disk = 0; disk < conf->raid_disks; disk++) {
		mdk_rdev_t *rdev = conf->multipaths[disk].rdev;
		if (rdev && rdev->in_sync)
			return disk;
	}
	BUG();
	return 0;
}

static void unplug_slaves(mddev_t *mddev)
{
	multipath_conf_t *conf = mddev_to_conf(mddev);
	int i;

	for (i=0; i<mddev->raid_disks; i++) {
		mdk_rdev_t *rdev = conf->multipaths[i].rdev;
		if (rdev && !rdev->faulty) {
			request_queue_t *r_queue = bdev_get_queue(rdev->bdev);

			if (r_queue->unplug_fn)
				r_queue->unplug_fn(r_queue);
		}
	}
}
static void multipath_unplug(request_queue_t *q)
{
	unplug_slaves(q->queuedata);
}


static int multipath_make_request (request_queue_t *q, struct bio * bio)
{
	mddev_t *mddev = q->queuedata;
	multipath_conf_t *conf = mddev_to_conf(mddev);
	struct multipath_bh * mp_bh;
	struct multipath_info *multipath;

	mp_bh = mempool_alloc(conf->pool, GFP_NOIO);

	mp_bh->master_bio = bio;
	mp_bh->mddev = mddev;

	if (bio_data_dir(bio)==WRITE) {
		disk_stat_inc(mddev->gendisk, writes);
		disk_stat_add(mddev->gendisk, write_sectors, bio_sectors(bio));
	} else {
		disk_stat_inc(mddev->gendisk, reads);
		disk_stat_add(mddev->gendisk, read_sectors, bio_sectors(bio));
	}
	/*
	 * read balancing logic:
	 */
	spin_lock_irq(&conf->device_lock);
	mp_bh->path = multipath_read_balance(conf);
	multipath = conf->multipaths + mp_bh->path;
	atomic_inc(&multipath->rdev->nr_pending);
	spin_unlock_irq(&conf->device_lock);

	mp_bh->bio = *bio;
	mp_bh->bio.bi_bdev = multipath->rdev->bdev;
	mp_bh->bio.bi_rw |= (1 << BIO_RW_FAILFAST);
	mp_bh->bio.bi_end_io = multipath_end_request;
	mp_bh->bio.bi_private = mp_bh;
	generic_make_request(&mp_bh->bio);
	return 0;
}

static void multipath_status (struct seq_file *seq, mddev_t *mddev)
{
	multipath_conf_t *conf = mddev_to_conf(mddev);
	int i;
	
	seq_printf (seq, " [%d/%d] [", conf->raid_disks,
						 conf->working_disks);
	for (i = 0; i < conf->raid_disks; i++)
		seq_printf (seq, "%s",
			       conf->multipaths[i].rdev && 
			       conf->multipaths[i].rdev->in_sync ? "U" : "_");
	seq_printf (seq, "]");
}


/*
 * Careful, this can execute in IRQ contexts as well!
 */
static void multipath_error (mddev_t *mddev, mdk_rdev_t *rdev)
{
	multipath_conf_t *conf = mddev_to_conf(mddev);

	if (conf->working_disks <= 1) {
		/*
		 * Uh oh, we can do nothing if this is our last path, but
		 * first check if this is a queued request for a device
		 * which has just failed.
		 */
		printk(KERN_ALERT 
			"multipath: only one IO path left and IO error.\n");
		/* leave it active... it's all we have */
	} else {
		/*
		 * Mark disk as unusable
		 */
		if (!rdev->faulty) {
			char b[BDEVNAME_SIZE];
			rdev->in_sync = 0;
			rdev->faulty = 1;
			mddev->sb_dirty = 1;
			conf->working_disks--;
			printk(KERN_ALERT "multipath: IO failure on %s,"
				" disabling IO path. \n	Operation continuing"
				" on %d IO paths.\n",
				bdevname (rdev->bdev,b),
				conf->working_disks);
		}
	}
}

static void print_multipath_conf (multipath_conf_t *conf)
{
	int i;
	struct multipath_info *tmp;

	printk("MULTIPATH conf printout:\n");
	if (!conf) {
		printk("(conf==NULL)\n");
		return;
	}
	printk(" --- wd:%d rd:%d\n", conf->working_disks,
			 conf->raid_disks);

	for (i = 0; i < conf->raid_disks; i++) {
		char b[BDEVNAME_SIZE];
		tmp = conf->multipaths + i;
		if (tmp->rdev)
			printk(" disk%d, o:%d, dev:%s\n",
				i,!tmp->rdev->faulty,
			       bdevname(tmp->rdev->bdev,b));
	}
}


static int multipath_add_disk(mddev_t *mddev, mdk_rdev_t *rdev)
{
	multipath_conf_t *conf = mddev->private;
	int found = 0;
	int path;
	struct multipath_info *p;

	print_multipath_conf(conf);
	spin_lock_irq(&conf->device_lock);
	for (path=0; path<mddev->raid_disks; path++) 
		if ((p=conf->multipaths+path)->rdev == NULL) {
			p->rdev = rdev;
			blk_queue_stack_limits(mddev->queue,
					       rdev->bdev->bd_disk->queue);

		/* as we don't honour merge_bvec_fn, we must never risk
		 * violating it, so limit ->max_sector to one PAGE, as
		 * a one page request is never in violation.
		 * (Note: it is very unlikely that a device with
		 * merge_bvec_fn will be involved in multipath.)
		 */
			if (rdev->bdev->bd_disk->queue->merge_bvec_fn &&
			    mddev->queue->max_sectors > (PAGE_SIZE>>9))
				mddev->queue->max_sectors = (PAGE_SIZE>>9);

			conf->working_disks++;
			rdev->raid_disk = path;
			rdev->in_sync = 1;
			found = 1;
		}
	spin_unlock_irq(&conf->device_lock);

	print_multipath_conf(conf);
	return found;
}

static int multipath_remove_disk(mddev_t *mddev, int number)
{
	multipath_conf_t *conf = mddev->private;
	int err = 1;
	struct multipath_info *p = conf->multipaths + number;

	print_multipath_conf(conf);
	spin_lock_irq(&conf->device_lock);

	if (p->rdev) {
		if (p->rdev->in_sync ||
		    atomic_read(&p->rdev->nr_pending)) {
			printk(KERN_ERR "hot-remove-disk, slot %d is identified"				" but is still operational!\n", number);
			err = -EBUSY;
			goto abort;
		}
		p->rdev = NULL;
		err = 0;
	}
	if (err)
		MD_BUG();
abort:
	spin_unlock_irq(&conf->device_lock);

	print_multipath_conf(conf);
	return err;
}



/*
 * This is a kernel thread which:
 *
 *	1.	Retries failed read operations on working multipaths.
 *	2.	Updates the raid superblock when problems encounter.
 *	3.	Performs writes following reads for array syncronising.
 */

static void multipathd (mddev_t *mddev)
{
	struct multipath_bh *mp_bh;
	struct bio *bio;
	unsigned long flags;
	mdk_rdev_t *rdev;

	md_check_recovery(mddev);
	for (;;) {
		char b[BDEVNAME_SIZE];
		spin_lock_irqsave(&retry_list_lock, flags);
		mp_bh = multipath_retry_list;
		if (!mp_bh)
			break;
		multipath_retry_list = mp_bh->next_mp;
		spin_unlock_irqrestore(&retry_list_lock, flags);

		mddev = mp_bh->mddev;
		bio = &mp_bh->bio;
		bio->bi_sector = mp_bh->master_bio->bi_sector;
		
		rdev = NULL;
		if (multipath_map (mddev, &rdev)<0) {
			printk(KERN_ALERT "multipath: %s: unrecoverable IO read"
				" error for block %llu\n",
				bdevname(bio->bi_bdev,b),
				(unsigned long long)bio->bi_sector);
			multipath_end_bh_io(mp_bh, 0);
		} else {
			printk(KERN_ERR "multipath: %s: redirecting sector %llu"
				" to another IO path\n",
				bdevname(bio->bi_bdev,b),
				(unsigned long long)bio->bi_sector);
			bio->bi_bdev = rdev->bdev;
			generic_make_request(bio);
		}
	}
	spin_unlock_irqrestore(&retry_list_lock, flags);
}

static int multipath_run (mddev_t *mddev)
{
	multipath_conf_t *conf;
	int disk_idx;
	struct multipath_info *disk;
	mdk_rdev_t *rdev;
	struct list_head *tmp;

	if (mddev->level != LEVEL_MULTIPATH) {
		printk("multipath: %s: raid level not set to multipath IO (%d)\n",
		       mdname(mddev), mddev->level);
		goto out;
	}
	/*
	 * copy the already verified devices into our private MULTIPATH
	 * bookkeeping area. [whatever we allocate in multipath_run(),
	 * should be freed in multipath_stop()]
	 */

	conf = kmalloc(sizeof(multipath_conf_t), GFP_KERNEL);
	mddev->private = conf;
	if (!conf) {
		printk(KERN_ERR 
			"multipath: couldn't allocate memory for %s\n",
			mdname(mddev));
		goto out;
	}
	memset(conf, 0, sizeof(*conf));

	conf->multipaths = kmalloc(sizeof(struct multipath_info)*mddev->raid_disks,
				   GFP_KERNEL);
	if (!conf->multipaths) {
		printk(KERN_ERR 
			"multipath: couldn't allocate memory for %s\n",
			mdname(mddev));
		goto out_free_conf;
	}
	memset(conf->multipaths, 0, sizeof(struct multipath_info)*mddev->raid_disks);

	mddev->queue->unplug_fn = multipath_unplug;

	conf->working_disks = 0;
	ITERATE_RDEV(mddev,rdev,tmp) {
		disk_idx = rdev->raid_disk;
		if (disk_idx < 0 ||
		    disk_idx >= mddev->raid_disks)
			continue;

		disk = conf->multipaths + disk_idx;
		disk->rdev = rdev;

		blk_queue_stack_limits(mddev->queue,
				       rdev->bdev->bd_disk->queue);
		/* as we don't honour merge_bvec_fn, we must never risk
		 * violating it, not that we ever expect a device with
		 * a merge_bvec_fn to be involved in multipath */
		if (rdev->bdev->bd_disk->queue->merge_bvec_fn &&
		    mddev->queue->max_sectors > (PAGE_SIZE>>9))
			mddev->queue->max_sectors = (PAGE_SIZE>>9);

		if (!rdev->faulty) 
			conf->working_disks++;
	}

	conf->raid_disks = mddev->raid_disks;
	mddev->sb_dirty = 1;
	conf->mddev = mddev;
	conf->device_lock = SPIN_LOCK_UNLOCKED;

	if (!conf->working_disks) {
		printk(KERN_ERR "multipath: no operational IO paths for %s\n",
			mdname(mddev));
		goto out_free_conf;
	}
	mddev->degraded = conf->raid_disks = conf->working_disks;

	conf->pool = mempool_create(NR_RESERVED_BUFS,
				    mp_pool_alloc, mp_pool_free,
				    NULL);
	if (conf->pool == NULL) {
		printk(KERN_ERR 
			"multipath: couldn't allocate memory for %s\n",
			mdname(mddev));
		goto out_free_conf;
	}

	{
		mddev->thread = md_register_thread(multipathd, mddev, "%s_multipath");
		if (!mddev->thread) {
			printk(KERN_ERR "multipath: couldn't allocate thread"
				" for %s\n", mdname(mddev));
			goto out_free_conf;
		}
	}

	printk(KERN_INFO 
		"multipath: array %s active with %d out of %d IO paths\n",
		mdname(mddev), conf->working_disks, mddev->raid_disks);
	/*
	 * Ok, everything is just fine now
	 */
	mddev->array_size = mddev->size;
	return 0;

out_free_conf:
	if (conf->pool)
		mempool_destroy(conf->pool);
	if (conf->multipaths)
		kfree(conf->multipaths);
	kfree(conf);
	mddev->private = NULL;
out:
	return -EIO;
}


static int multipath_stop (mddev_t *mddev)
{
	multipath_conf_t *conf = mddev_to_conf(mddev);

	md_unregister_thread(mddev->thread);
	mddev->thread = NULL;
	mempool_destroy(conf->pool);
	kfree(conf->multipaths);
	kfree(conf);
	mddev->private = NULL;
	return 0;
}

static mdk_personality_t multipath_personality=
{
	.name		= "multipath",
	.owner		= THIS_MODULE,
	.make_request	= multipath_make_request,
	.run		= multipath_run,
	.stop		= multipath_stop,
	.status		= multipath_status,
	.error_handler	= multipath_error,
	.hot_add_disk	= multipath_add_disk,
	.hot_remove_disk= multipath_remove_disk,
};

static int __init multipath_init (void)
{
	return register_md_personality (MULTIPATH, &multipath_personality);
}

static void __exit multipath_exit (void)
{
	unregister_md_personality (MULTIPATH);
}

module_init(multipath_init);
module_exit(multipath_exit);
MODULE_LICENSE("GPL");
MODULE_ALIAS("md-personality-7"); /* MULTIPATH */
