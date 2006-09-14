/*
 * raid1.c : Multiple Devices driver for Linux
 *
 * Copyright (C) 1999, 2000, 2001 Ingo Molnar, Red Hat
 *
 * Copyright (C) 1996, 1997, 1998 Ingo Molnar, Miguel de Icaza, Gadi Oxman
 *
 * RAID-1 management functions.
 *
 * Better read-balancing code written by Mika Kuoppala <miku@iki.fi>, 2000
 *
 * Fixes to reconstruction by Jakob Østergaard" <jakob@ostenfeld.dk>
 * Various fixes by Neil Brown <neilb@cse.unsw.edu.au>
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

#include <linux/raid/raid1.h>

#define MAJOR_NR MD_MAJOR
#define MD_DRIVER
#define MD_PERSONALITY

/*
 * Number of guaranteed r1bios in case of extreme VM load:
 */
#define	NR_RAID1_BIOS 256

static mdk_personality_t raid1_personality;
static spinlock_t retry_list_lock = SPIN_LOCK_UNLOCKED;
static LIST_HEAD(retry_list_head);

static void unplug_slaves(mddev_t *mddev);


static void * r1bio_pool_alloc(int gfp_flags, void *data)
{
	mddev_t *mddev = data;
	r1bio_t *r1_bio;

	/* allocate a r1bio with room for raid_disks entries in the bios array */
	r1_bio = kmalloc(sizeof(r1bio_t) + sizeof(struct bio*)*mddev->raid_disks,
			 gfp_flags);
	if (r1_bio)
		memset(r1_bio, 0, sizeof(*r1_bio) + sizeof(struct bio*)*mddev->raid_disks);
	else
		unplug_slaves(mddev);

	return r1_bio;
}

static void r1bio_pool_free(void *r1_bio, void *data)
{
	kfree(r1_bio);
}

#define RESYNC_BLOCK_SIZE (64*1024)
//#define RESYNC_BLOCK_SIZE PAGE_SIZE
#define RESYNC_SECTORS (RESYNC_BLOCK_SIZE >> 9)
#define RESYNC_PAGES ((RESYNC_BLOCK_SIZE + PAGE_SIZE-1) / PAGE_SIZE)
#define RESYNC_WINDOW (2048*1024)

static void * r1buf_pool_alloc(int gfp_flags, void *data)
{
	conf_t *conf = data;
	struct page *page;
	r1bio_t *r1_bio;
	struct bio *bio;
	int i, j;

	r1_bio = r1bio_pool_alloc(gfp_flags, conf->mddev);
	if (!r1_bio) {
		unplug_slaves(conf->mddev);
		return NULL;
	}

	/*
	 * Allocate bios : 1 for reading, n-1 for writing
	 */
	for (j = conf->raid_disks ; j-- ; ) {
		bio = bio_alloc(gfp_flags, RESYNC_PAGES);
		if (!bio)
			goto out_free_bio;
		r1_bio->bios[j] = bio;
	}
	/*
	 * Allocate RESYNC_PAGES data pages and attach them to
	 * the first bio;
	 */
	bio = r1_bio->bios[0];
	for (i = 0; i < RESYNC_PAGES; i++) {
		page = alloc_page(gfp_flags);
		if (unlikely(!page))
			goto out_free_pages;

		bio->bi_io_vec[i].bv_page = page;
	}

	r1_bio->master_bio = bio;

	return r1_bio;

out_free_pages:
	for ( ; i > 0 ; i--)
		__free_page(bio->bi_io_vec[i-1].bv_page);
out_free_bio:
	while ( ++j < conf->raid_disks )
		bio_put(r1_bio->bios[j]);
	r1bio_pool_free(r1_bio, conf->mddev);
	return NULL;
}

static void r1buf_pool_free(void *__r1_bio, void *data)
{
	int i;
	conf_t *conf = data;
	r1bio_t *r1bio = __r1_bio;
	struct bio *bio = r1bio->bios[0];

	for (i = 0; i < RESYNC_PAGES; i++) {
		__free_page(bio->bi_io_vec[i].bv_page);
		bio->bi_io_vec[i].bv_page = NULL;
	}
	for (i=0 ; i < conf->raid_disks; i++)
		bio_put(r1bio->bios[i]);

	r1bio_pool_free(r1bio, conf->mddev);
}

static void put_all_bios(conf_t *conf, r1bio_t *r1_bio)
{
	int i;

	for (i = 0; i < conf->raid_disks; i++) {
		struct bio **bio = r1_bio->bios + i;
		if (*bio)
			bio_put(*bio);
		*bio = NULL;
	}
}

static inline void free_r1bio(r1bio_t *r1_bio)
{
	unsigned long flags;

	conf_t *conf = mddev_to_conf(r1_bio->mddev);

	/*
	 * Wake up any possible resync thread that waits for the device
	 * to go idle.
	 */
	spin_lock_irqsave(&conf->resync_lock, flags);
	if (!--conf->nr_pending) {
		wake_up(&conf->wait_idle);
		wake_up(&conf->wait_resume);
	}
	spin_unlock_irqrestore(&conf->resync_lock, flags);

	put_all_bios(conf, r1_bio);
	mempool_free(r1_bio, conf->r1bio_pool);
}

static inline void put_buf(r1bio_t *r1_bio)
{
	conf_t *conf = mddev_to_conf(r1_bio->mddev);
	unsigned long flags;

	mempool_free(r1_bio, conf->r1buf_pool);

	spin_lock_irqsave(&conf->resync_lock, flags);
	if (!conf->barrier)
		BUG();
	--conf->barrier;
	wake_up(&conf->wait_resume);
	wake_up(&conf->wait_idle);

	if (!--conf->nr_pending) {
		wake_up(&conf->wait_idle);
		wake_up(&conf->wait_resume);
	}
	spin_unlock_irqrestore(&conf->resync_lock, flags);
}

static int map(mddev_t *mddev, mdk_rdev_t **rdevp)
{
	conf_t *conf = mddev_to_conf(mddev);
	int i, disks = conf->raid_disks;

	/*
	 * Later we do read balancing on the read side
	 * now we use the first available disk.
	 */

	spin_lock_irq(&conf->device_lock);
	for (i = 0; i < disks; i++) {
		mdk_rdev_t *rdev = conf->mirrors[i].rdev;
		if (rdev && rdev->in_sync) {
			*rdevp = rdev;
			atomic_inc(&rdev->nr_pending);
			spin_unlock_irq(&conf->device_lock);
			return 0;
		}
	}
	spin_unlock_irq(&conf->device_lock);

	printk(KERN_ERR "raid1_map(): huh, no more operational devices?\n");
	return -1;
}

static void reschedule_retry(r1bio_t *r1_bio)
{
	unsigned long flags;
	mddev_t *mddev = r1_bio->mddev;

	spin_lock_irqsave(&retry_list_lock, flags);
	list_add(&r1_bio->retry_list, &retry_list_head);
	spin_unlock_irqrestore(&retry_list_lock, flags);

	md_wakeup_thread(mddev->thread);
}

/*
 * raid_end_bio_io() is called when we have finished servicing a mirrored
 * operation and are ready to return a success/failure code to the buffer
 * cache layer.
 */
static void raid_end_bio_io(r1bio_t *r1_bio)
{
	struct bio *bio = r1_bio->master_bio;

	bio_endio(bio, bio->bi_size,
		test_bit(R1BIO_Uptodate, &r1_bio->state) ? 0 : -EIO);
	free_r1bio(r1_bio);
}

/*
 * Update disk head position estimator based on IRQ completion info.
 */
static inline void update_head_pos(int disk, r1bio_t *r1_bio)
{
	conf_t *conf = mddev_to_conf(r1_bio->mddev);

	conf->mirrors[disk].head_position =
		r1_bio->sector + (r1_bio->sectors);
}

static int raid1_end_read_request(struct bio *bio, unsigned int bytes_done, int error)
{
	int uptodate = test_bit(BIO_UPTODATE, &bio->bi_flags);
	r1bio_t * r1_bio = (r1bio_t *)(bio->bi_private);
	int mirror;
	conf_t *conf = mddev_to_conf(r1_bio->mddev);

	if (bio->bi_size)
		return 1;
	
	mirror = r1_bio->read_disk;
	/*
	 * this branch is our 'one mirror IO has finished' event handler:
	 */
	if (!uptodate)
		md_error(r1_bio->mddev, conf->mirrors[mirror].rdev);
	else
		/*
		 * Set R1BIO_Uptodate in our master bio, so that
		 * we will return a good error code for to the higher
		 * levels even if IO on some other mirrored buffer fails.
		 *
		 * The 'master' represents the composite IO operation to
		 * user-side. So if something waits for IO, then it will
		 * wait for the 'master' bio.
		 */
		set_bit(R1BIO_Uptodate, &r1_bio->state);

	update_head_pos(mirror, r1_bio);

	/*
	 * we have only one bio on the read side
	 */
	if (uptodate)
		raid_end_bio_io(r1_bio);
	else {
		/*
		 * oops, read error:
		 */
		char b[BDEVNAME_SIZE];
		printk(KERN_ERR "raid1: %s: rescheduling sector %llu\n",
		       bdevname(conf->mirrors[mirror].rdev->bdev,b), (unsigned long long)r1_bio->sector);
		reschedule_retry(r1_bio);
	}

	atomic_dec(&conf->mirrors[mirror].rdev->nr_pending);
	return 0;
}

static int raid1_end_write_request(struct bio *bio, unsigned int bytes_done, int error)
{
	int uptodate = test_bit(BIO_UPTODATE, &bio->bi_flags);
	r1bio_t * r1_bio = (r1bio_t *)(bio->bi_private);
	int mirror;
	conf_t *conf = mddev_to_conf(r1_bio->mddev);

	if (bio->bi_size)
		return 1;

	for (mirror = 0; mirror < conf->raid_disks; mirror++)
		if (r1_bio->bios[mirror] == bio)
			break;

	/*
	 * this branch is our 'one mirror IO has finished' event handler:
	 */
	if (!uptodate)
		md_error(r1_bio->mddev, conf->mirrors[mirror].rdev);
	else
		/*
		 * Set R1BIO_Uptodate in our master bio, so that
		 * we will return a good error code for to the higher
		 * levels even if IO on some other mirrored buffer fails.
		 *
		 * The 'master' represents the composite IO operation to
		 * user-side. So if something waits for IO, then it will
		 * wait for the 'master' bio.
		 */
		set_bit(R1BIO_Uptodate, &r1_bio->state);

	update_head_pos(mirror, r1_bio);

	/*
	 *
	 * Let's see if all mirrored write operations have finished
	 * already.
	 */
	if (atomic_dec_and_test(&r1_bio->remaining)) {
		md_write_end(r1_bio->mddev);
		raid_end_bio_io(r1_bio);
	}

	atomic_dec(&conf->mirrors[mirror].rdev->nr_pending);
	return 0;
}


/*
 * This routine returns the disk from which the requested read should
 * be done. There is a per-array 'next expected sequential IO' sector
 * number - if this matches on the next IO then we use the last disk.
 * There is also a per-disk 'last know head position' sector that is
 * maintained from IRQ contexts, both the normal and the resync IO
 * completion handlers update this position correctly. If there is no
 * perfect sequential match then we pick the disk whose head is closest.
 *
 * If there are 2 mirrors in the same 2 devices, performance degrades
 * because position is mirror, not device based.
 *
 * The rdev for the device selected will have nr_pending incremented.
 */
static int read_balance(conf_t *conf, struct bio *bio, r1bio_t *r1_bio)
{
	const unsigned long this_sector = r1_bio->sector;
	int new_disk = conf->last_used, disk = new_disk;
	const int sectors = bio->bi_size >> 9;
	sector_t new_distance, current_distance;

	spin_lock_irq(&conf->device_lock);
	/*
	 * Check if it if we can balance. We can balance on the whole
	 * device if no resync is going on, or below the resync window.
	 * We take the first readable disk when above the resync window.
	 */
	if (!conf->mddev->in_sync && (this_sector + sectors >= conf->next_resync)) {
		/* make sure that disk is operational */
		new_disk = 0;

		while (!conf->mirrors[new_disk].rdev ||
		       !conf->mirrors[new_disk].rdev->in_sync) {
			new_disk++;
			if (new_disk == conf->raid_disks) {
				new_disk = 0;
				break;
			}
		}
		goto rb_out;
	}


	/* make sure the disk is operational */
	while (!conf->mirrors[new_disk].rdev ||
	       !conf->mirrors[new_disk].rdev->in_sync) {
		if (new_disk <= 0)
			new_disk = conf->raid_disks;
		new_disk--;
		if (new_disk == disk) {
			new_disk = conf->last_used;
			goto rb_out;
		}
	}
	disk = new_disk;
	/* now disk == new_disk == starting point for search */

	/*
	 * Don't change to another disk for sequential reads:
	 */
	if (conf->next_seq_sect == this_sector)
		goto rb_out;
	if (this_sector == conf->mirrors[new_disk].head_position)
		goto rb_out;

	current_distance = abs(this_sector - conf->mirrors[disk].head_position);

	/* Find the disk whose head is closest */

	do {
		if (disk <= 0)
			disk = conf->raid_disks;
		disk--;

		if (!conf->mirrors[disk].rdev ||
		    !conf->mirrors[disk].rdev->in_sync)
			continue;

		if (!atomic_read(&conf->mirrors[disk].rdev->nr_pending)) {
			new_disk = disk;
			break;
		}
		new_distance = abs(this_sector - conf->mirrors[disk].head_position);
		if (new_distance < current_distance) {
			current_distance = new_distance;
			new_disk = disk;
		}
	} while (disk != conf->last_used);

rb_out:
	r1_bio->read_disk = new_disk;
	conf->next_seq_sect = this_sector + sectors;

	conf->last_used = new_disk;

	if (conf->mirrors[new_disk].rdev)
		atomic_inc(&conf->mirrors[new_disk].rdev->nr_pending);
	spin_unlock_irq(&conf->device_lock);

	return new_disk;
}

static void unplug_slaves(mddev_t *mddev)
{
	conf_t *conf = mddev_to_conf(mddev);
	int i;
	unsigned long flags;

	spin_lock_irqsave(&conf->device_lock, flags);
	for (i=0; i<mddev->raid_disks; i++) {
		mdk_rdev_t *rdev = conf->mirrors[i].rdev;
		if (rdev && !rdev->faulty) {
			request_queue_t *r_queue = bdev_get_queue(rdev->bdev);

			if (r_queue->unplug_fn)
				r_queue->unplug_fn(r_queue);
		}
	}
	spin_unlock_irqrestore(&conf->device_lock, flags);
}
static void raid1_unplug(request_queue_t *q)
{
	unplug_slaves(q->queuedata);
}

/*
 * Throttle resync depth, so that we can both get proper overlapping of
 * requests, but are still able to handle normal requests quickly.
 */
#define RESYNC_DEPTH 32

static void device_barrier(conf_t *conf, sector_t sect)
{
	spin_lock_irq(&conf->resync_lock);
	wait_event_lock_irq(conf->wait_idle, !waitqueue_active(&conf->wait_resume),
			    conf->resync_lock, unplug_slaves(conf->mddev));
	
	if (!conf->barrier++) {
		wait_event_lock_irq(conf->wait_idle, !conf->nr_pending,
				    conf->resync_lock, unplug_slaves(conf->mddev));
		if (conf->nr_pending)
			BUG();
	}
	wait_event_lock_irq(conf->wait_resume, conf->barrier < RESYNC_DEPTH,
			    conf->resync_lock, unplug_slaves(conf->mddev));
	conf->next_resync = sect;
	spin_unlock_irq(&conf->resync_lock);
}

static int make_request(request_queue_t *q, struct bio * bio)
{
	mddev_t *mddev = q->queuedata;
	conf_t *conf = mddev_to_conf(mddev);
	mirror_info_t *mirror;
	r1bio_t *r1_bio;
	struct bio *read_bio;
	int i, disks = conf->raid_disks;

	/*
	 * Register the new request and wait if the reconstruction
	 * thread has put up a bar for new requests.
	 * Continue immediately if no resync is active currently.
	 */
	spin_lock_irq(&conf->resync_lock);
	wait_event_lock_irq(conf->wait_resume, !conf->barrier, conf->resync_lock, );
	conf->nr_pending++;
	spin_unlock_irq(&conf->resync_lock);

	if (bio_data_dir(bio)==WRITE) {
		disk_stat_inc(mddev->gendisk, writes);
		disk_stat_add(mddev->gendisk, write_sectors, bio_sectors(bio));
	} else {
		disk_stat_inc(mddev->gendisk, reads);
		disk_stat_add(mddev->gendisk, read_sectors, bio_sectors(bio));
	}

	/*
	 * make_request() can abort the operation when READA is being
	 * used and no empty request is available.
	 *
	 */
	r1_bio = mempool_alloc(conf->r1bio_pool, GFP_NOIO);

	r1_bio->master_bio = bio;
	r1_bio->sectors = bio->bi_size >> 9;

	r1_bio->mddev = mddev;
	r1_bio->sector = bio->bi_sector;

	if (bio_data_dir(bio) == READ) {
		/*
		 * read balancing logic:
		 */
		mirror = conf->mirrors + read_balance(conf, bio, r1_bio);

		read_bio = bio_clone(bio, GFP_NOIO);

		r1_bio->bios[r1_bio->read_disk] = read_bio;

		read_bio->bi_sector = r1_bio->sector + mirror->rdev->data_offset;
		read_bio->bi_bdev = mirror->rdev->bdev;
		read_bio->bi_end_io = raid1_end_read_request;
		read_bio->bi_rw = READ;
		read_bio->bi_private = r1_bio;

		generic_make_request(read_bio);
		return 0;
	}

	/*
	 * WRITE:
	 */
	/* first select target devices under spinlock and
	 * inc refcount on their rdev.  Record them by setting
	 * bios[x] to bio
	 */
	spin_lock_irq(&conf->device_lock);
	for (i = 0;  i < disks; i++) {
		if (conf->mirrors[i].rdev &&
		    !conf->mirrors[i].rdev->faulty) {
			atomic_inc(&conf->mirrors[i].rdev->nr_pending);
			r1_bio->bios[i] = bio;
		} else
			r1_bio->bios[i] = NULL;
	}
	spin_unlock_irq(&conf->device_lock);

	atomic_set(&r1_bio->remaining, 1);
	md_write_start(mddev);
	for (i = 0; i < disks; i++) {
		struct bio *mbio;
		if (!r1_bio->bios[i])
			continue;

		mbio = bio_clone(bio, GFP_NOIO);
		r1_bio->bios[i] = mbio;

		mbio->bi_sector	= r1_bio->sector + conf->mirrors[i].rdev->data_offset;
		mbio->bi_bdev = conf->mirrors[i].rdev->bdev;
		mbio->bi_end_io	= raid1_end_write_request;
		mbio->bi_rw = WRITE;
		mbio->bi_private = r1_bio;

		atomic_inc(&r1_bio->remaining);
		generic_make_request(mbio);
	}

	if (atomic_dec_and_test(&r1_bio->remaining)) {
		md_write_end(mddev);
		raid_end_bio_io(r1_bio);
	}

	return 0;
}

static void status(struct seq_file *seq, mddev_t *mddev)
{
	conf_t *conf = mddev_to_conf(mddev);
	int i;

	seq_printf(seq, " [%d/%d] [", conf->raid_disks,
						conf->working_disks);
	for (i = 0; i < conf->raid_disks; i++)
		seq_printf(seq, "%s",
			      conf->mirrors[i].rdev &&
			      conf->mirrors[i].rdev->in_sync ? "U" : "_");
	seq_printf(seq, "]");
}


static void error(mddev_t *mddev, mdk_rdev_t *rdev)
{
	char b[BDEVNAME_SIZE];
	conf_t *conf = mddev_to_conf(mddev);

	/*
	 * If it is not operational, then we have already marked it as dead
	 * else if it is the last working disks, ignore the error, let the
	 * next level up know.
	 * else mark the drive as failed
	 */
	if (rdev->in_sync
	    && conf->working_disks == 1)
		/*
		 * Don't fail the drive, act as though we were just a
		 * normal single drive
		 */
		return;
	if (rdev->in_sync) {
		mddev->degraded++;
		conf->working_disks--;
		/*
		 * if recovery is running, make sure it aborts.
		 */
		set_bit(MD_RECOVERY_ERR, &mddev->recovery);
	}
	rdev->in_sync = 0;
	rdev->faulty = 1;
	mddev->sb_dirty = 1;
	printk(KERN_ALERT "raid1: Disk failure on %s, disabling device. \n"
		"	Operation continuing on %d devices\n",
		bdevname(rdev->bdev,b), conf->working_disks);
}

static void print_conf(conf_t *conf)
{
	int i;
	mirror_info_t *tmp;

	printk("RAID1 conf printout:\n");
	if (!conf) {
		printk("(!conf)\n");
		return;
	}
	printk(" --- wd:%d rd:%d\n", conf->working_disks,
		conf->raid_disks);

	for (i = 0; i < conf->raid_disks; i++) {
		char b[BDEVNAME_SIZE];
		tmp = conf->mirrors + i;
		if (tmp->rdev)
			printk(" disk %d, wo:%d, o:%d, dev:%s\n",
				i, !tmp->rdev->in_sync, !tmp->rdev->faulty,
				bdevname(tmp->rdev->bdev,b));
	}
}

static void close_sync(conf_t *conf)
{
	spin_lock_irq(&conf->resync_lock);
	wait_event_lock_irq(conf->wait_resume, !conf->barrier,
			    conf->resync_lock, 	unplug_slaves(conf->mddev));
	spin_unlock_irq(&conf->resync_lock);

	if (conf->barrier) BUG();
	if (waitqueue_active(&conf->wait_idle)) BUG();

	mempool_destroy(conf->r1buf_pool);
	conf->r1buf_pool = NULL;
}

static int raid1_spare_active(mddev_t *mddev)
{
	int i;
	conf_t *conf = mddev->private;
	mirror_info_t *tmp;

	spin_lock_irq(&conf->device_lock);
	/*
	 * Find all failed disks within the RAID1 configuration 
	 * and mark them readable
	 */
	for (i = 0; i < conf->raid_disks; i++) {
		tmp = conf->mirrors + i;
		if (tmp->rdev 
		    && !tmp->rdev->faulty
		    && !tmp->rdev->in_sync) {
			conf->working_disks++;
			mddev->degraded--;
			tmp->rdev->in_sync = 1;
		}
	}
	spin_unlock_irq(&conf->device_lock);

	print_conf(conf);
	return 0;
}


static int raid1_add_disk(mddev_t *mddev, mdk_rdev_t *rdev)
{
	conf_t *conf = mddev->private;
	int found = 0;
	int mirror;
	mirror_info_t *p;

	spin_lock_irq(&conf->device_lock);
	for (mirror=0; mirror < mddev->raid_disks; mirror++)
		if ( !(p=conf->mirrors+mirror)->rdev) {
			p->rdev = rdev;

			blk_queue_stack_limits(mddev->queue,
					       rdev->bdev->bd_disk->queue);
			/* as we don't honour merge_bvec_fn, we must never risk
			 * violating it, so limit ->max_sector to one PAGE, as
			 * a one page request is never in violation.
			 */
			if (rdev->bdev->bd_disk->queue->merge_bvec_fn &&
			    mddev->queue->max_sectors > (PAGE_SIZE>>9))
				mddev->queue->max_sectors = (PAGE_SIZE>>9);

			p->head_position = 0;
			rdev->raid_disk = mirror;
			found = 1;
			break;
		}
	spin_unlock_irq(&conf->device_lock);

	print_conf(conf);
	return found;
}

static int raid1_remove_disk(mddev_t *mddev, int number)
{
	conf_t *conf = mddev->private;
	int err = 1;
	mirror_info_t *p = conf->mirrors+ number;

	print_conf(conf);
	spin_lock_irq(&conf->device_lock);
	if (p->rdev) {
		if (p->rdev->in_sync ||
		    atomic_read(&p->rdev->nr_pending)) {
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

	print_conf(conf);
	return err;
}


static int end_sync_read(struct bio *bio, unsigned int bytes_done, int error)
{
	int uptodate = test_bit(BIO_UPTODATE, &bio->bi_flags);
	r1bio_t * r1_bio = (r1bio_t *)(bio->bi_private);
	conf_t *conf = mddev_to_conf(r1_bio->mddev);

	if (bio->bi_size)
		return 1;

	if (r1_bio->bios[r1_bio->read_disk] != bio)
		BUG();
	update_head_pos(r1_bio->read_disk, r1_bio);
	/*
	 * we have read a block, now it needs to be re-written,
	 * or re-read if the read failed.
	 * We don't do much here, just schedule handling by raid1d
	 */
	if (!uptodate)
		md_error(r1_bio->mddev,
			 conf->mirrors[r1_bio->read_disk].rdev);
	else
		set_bit(R1BIO_Uptodate, &r1_bio->state);
	atomic_dec(&conf->mirrors[r1_bio->read_disk].rdev->nr_pending);
	reschedule_retry(r1_bio);
	return 0;
}

static int end_sync_write(struct bio *bio, unsigned int bytes_done, int error)
{
	int uptodate = test_bit(BIO_UPTODATE, &bio->bi_flags);
	r1bio_t * r1_bio = (r1bio_t *)(bio->bi_private);
	mddev_t *mddev = r1_bio->mddev;
	conf_t *conf = mddev_to_conf(mddev);
	int i;
	int mirror=0;

	if (bio->bi_size)
		return 1;

	for (i = 0; i < conf->raid_disks; i++)
		if (r1_bio->bios[i] == bio) {
			mirror = i;
			break;
		}
	if (!uptodate)
		md_error(mddev, conf->mirrors[mirror].rdev);
	update_head_pos(mirror, r1_bio);

	if (atomic_dec_and_test(&r1_bio->remaining)) {
		md_done_sync(mddev, r1_bio->sectors, uptodate);
		put_buf(r1_bio);
	}
	atomic_dec(&conf->mirrors[mirror].rdev->nr_pending);
	return 0;
}

static void sync_request_write(mddev_t *mddev, r1bio_t *r1_bio)
{
	conf_t *conf = mddev_to_conf(mddev);
	int i;
	int disks = conf->raid_disks;
	struct bio *bio, *wbio;

	bio = r1_bio->bios[r1_bio->read_disk];

	/*
	 * schedule writes
	 */
	if (!test_bit(R1BIO_Uptodate, &r1_bio->state)) {
		/*
		 * There is no point trying a read-for-reconstruct as
		 * reconstruct is about to be aborted
		 */
		char b[BDEVNAME_SIZE];
		printk(KERN_ALERT "raid1: %s: unrecoverable I/O read error"
			" for block %llu\n",
			bdevname(bio->bi_bdev,b), 
			(unsigned long long)r1_bio->sector);
		md_done_sync(mddev, r1_bio->sectors, 0);
		put_buf(r1_bio);
		return;
	}

	atomic_set(&r1_bio->remaining, 1);
	for (i = 0; i < disks ; i++) {
		wbio = r1_bio->bios[i];
		if (wbio->bi_end_io != end_sync_write)
			continue;

		atomic_inc(&conf->mirrors[i].rdev->nr_pending);
		atomic_inc(&r1_bio->remaining);
		md_sync_acct(conf->mirrors[i].rdev, wbio->bi_size >> 9);
		generic_make_request(wbio);
	}

	if (atomic_dec_and_test(&r1_bio->remaining)) {
		md_done_sync(mddev, r1_bio->sectors, 1);
		put_buf(r1_bio);
	}
}

/*
 * This is a kernel thread which:
 *
 *	1.	Retries failed read operations on working mirrors.
 *	2.	Updates the raid superblock when problems encounter.
 *	3.	Performs writes following reads for array syncronising.
 */

static void raid1d(mddev_t *mddev)
{
	struct list_head *head = &retry_list_head;
	r1bio_t *r1_bio;
	struct bio *bio;
	unsigned long flags;
	conf_t *conf = mddev_to_conf(mddev);
	int unplug=0;
	mdk_rdev_t *rdev;

	md_check_recovery(mddev);
	md_handle_safemode(mddev);
	
	for (;;) {
		char b[BDEVNAME_SIZE];
		spin_lock_irqsave(&retry_list_lock, flags);
		if (list_empty(head))
			break;
		r1_bio = list_entry(head->prev, r1bio_t, retry_list);
		list_del(head->prev);
		spin_unlock_irqrestore(&retry_list_lock, flags);

		mddev = r1_bio->mddev;
		conf = mddev_to_conf(mddev);
		bio = r1_bio->master_bio;
		if (test_bit(R1BIO_IsSync, &r1_bio->state)) {
			sync_request_write(mddev, r1_bio);
			unplug = 1;
		} else {
			if (map(mddev, &rdev) == -1) {
				printk(KERN_ALERT "raid1: %s: unrecoverable I/O"
				       " read error for block %llu\n",
				       bdevname(bio->bi_bdev,b),
				       (unsigned long long)r1_bio->sector);
				raid_end_bio_io(r1_bio);
			} else {
				printk(KERN_ERR "raid1: %s: redirecting sector %llu to"
				       " another mirror\n",
				       bdevname(rdev->bdev,b),
				       (unsigned long long)r1_bio->sector);
				bio->bi_bdev = rdev->bdev;
				bio->bi_sector = r1_bio->sector + rdev->data_offset;
				bio->bi_rw = READ;
				unplug = 1;
				generic_make_request(bio);
			}
		}
	}
	spin_unlock_irqrestore(&retry_list_lock, flags);
	if (unplug)
		unplug_slaves(mddev);
}


static int init_resync(conf_t *conf)
{
	int buffs;

	buffs = RESYNC_WINDOW / RESYNC_BLOCK_SIZE;
	if (conf->r1buf_pool)
		BUG();
	conf->r1buf_pool = mempool_create(buffs, r1buf_pool_alloc, r1buf_pool_free, conf);
	if (!conf->r1buf_pool)
		return -ENOMEM;
	conf->next_resync = 0;
	return 0;
}

/*
 * perform a "sync" on one "block"
 *
 * We need to make sure that no normal I/O request - particularly write
 * requests - conflict with active sync requests.
 *
 * This is achieved by tracking pending requests and a 'barrier' concept
 * that can be installed to exclude normal IO requests.
 */

static int sync_request(mddev_t *mddev, sector_t sector_nr, int go_faster)
{
	conf_t *conf = mddev_to_conf(mddev);
	mirror_info_t *mirror;
	r1bio_t *r1_bio;
	struct bio *bio;
	sector_t max_sector, nr_sectors;
	int disk;
	int i;

	if (!conf->r1buf_pool)
		if (init_resync(conf))
			return -ENOMEM;

	max_sector = mddev->size << 1;
	if (sector_nr >= max_sector) {
		close_sync(conf);
		return 0;
	}

	/*
	 * If there is non-resync activity waiting for us then
	 * put in a delay to throttle resync.
	 */
	if (!go_faster && waitqueue_active(&conf->wait_resume))
		schedule_timeout(HZ);
	device_barrier(conf, sector_nr + RESYNC_SECTORS);

	/*
	 * If reconstructing, and >1 working disc,
	 * could dedicate one to rebuild and others to
	 * service read requests ..
	 */
	disk = conf->last_used;
	/* make sure disk is operational */
	spin_lock_irq(&conf->device_lock);
	while (conf->mirrors[disk].rdev == NULL ||
	       !conf->mirrors[disk].rdev->in_sync) {
		if (disk <= 0)
			disk = conf->raid_disks;
		disk--;
		if (disk == conf->last_used)
			break;
	}
	conf->last_used = disk;
	atomic_inc(&conf->mirrors[disk].rdev->nr_pending);
	spin_unlock_irq(&conf->device_lock);

	mirror = conf->mirrors + disk;

	r1_bio = mempool_alloc(conf->r1buf_pool, GFP_NOIO);

	spin_lock_irq(&conf->resync_lock);
	conf->nr_pending++;
	spin_unlock_irq(&conf->resync_lock);

	r1_bio->mddev = mddev;
	r1_bio->sector = sector_nr;
	set_bit(R1BIO_IsSync, &r1_bio->state);
	r1_bio->read_disk = disk;

	for (i=0; i < conf->raid_disks; i++) {
		bio = r1_bio->bios[i];

		/* take from bio_init */
		bio->bi_next = NULL;
		bio->bi_flags |= 1 << BIO_UPTODATE;
		bio->bi_rw = 0;
		bio->bi_vcnt = 0;
		bio->bi_idx = 0;
		bio->bi_phys_segments = 0;
		bio->bi_hw_segments = 0;
		bio->bi_size = 0;
		bio->bi_end_io = NULL;
		bio->bi_private = NULL;

		if (i == disk) {
			bio->bi_rw = READ;
			bio->bi_end_io = end_sync_read;
		} else if (conf->mirrors[i].rdev &&
			   !conf->mirrors[i].rdev->faulty &&
			   (!conf->mirrors[i].rdev->in_sync ||
			    sector_nr + RESYNC_SECTORS > mddev->recovery_cp)) {
			bio->bi_rw = WRITE;
			bio->bi_end_io = end_sync_write;
		} else
			continue;
		bio->bi_sector = sector_nr + conf->mirrors[i].rdev->data_offset;
		bio->bi_bdev = conf->mirrors[i].rdev->bdev;
		bio->bi_private = r1_bio;
	}
	nr_sectors = 0;
	do {
		struct page *page;
		int len = PAGE_SIZE;
		if (sector_nr + (len>>9) > max_sector)
			len = (max_sector - sector_nr) << 9;
		if (len == 0)
			break;
		for (i=0 ; i < conf->raid_disks; i++) {
			bio = r1_bio->bios[i];
			if (bio->bi_end_io) {
				page = r1_bio->bios[0]->bi_io_vec[bio->bi_vcnt].bv_page;
				if (bio_add_page(bio, page, len, 0) == 0) {
					/* stop here */
					r1_bio->bios[0]->bi_io_vec[bio->bi_vcnt].bv_page = page;
					while (i > 0) {
						i--;
						bio = r1_bio->bios[i];
						if (bio->bi_end_io==NULL) continue;
						/* remove last page from this bio */
						bio->bi_vcnt--;
						bio->bi_size -= len;
						bio->bi_flags &= ~(1<< BIO_SEG_VALID);
					}
					goto bio_full;
				}
			}
		}
		nr_sectors += len>>9;
		sector_nr += len>>9;
	} while (r1_bio->bios[disk]->bi_vcnt < RESYNC_PAGES);
 bio_full:
	bio = r1_bio->bios[disk];
	r1_bio->sectors = nr_sectors;

	md_sync_acct(mirror->rdev, nr_sectors);

	generic_make_request(bio);

	return nr_sectors;
}

static int run(mddev_t *mddev)
{
	conf_t *conf;
	int i, j, disk_idx;
	mirror_info_t *disk;
	mdk_rdev_t *rdev;
	struct list_head *tmp;

	if (mddev->level != 1) {
		printk("raid1: %s: raid level not set to mirroring (%d)\n",
		       mdname(mddev), mddev->level);
		goto out;
	}
	/*
	 * copy the already verified devices into our private RAID1
	 * bookkeeping area. [whatever we allocate in run(),
	 * should be freed in stop()]
	 */
	conf = kmalloc(sizeof(conf_t), GFP_KERNEL);
	mddev->private = conf;
	if (!conf) {
		printk(KERN_ERR "raid1: couldn't allocate memory for %s\n",
			mdname(mddev));
		goto out;
	}
	memset(conf, 0, sizeof(*conf));
	conf->mirrors = kmalloc(sizeof(struct mirror_info)*mddev->raid_disks, 
				 GFP_KERNEL);
	if (!conf->mirrors) {
		printk(KERN_ERR "raid1: couldn't allocate memory for %s\n",
		       mdname(mddev));
		goto out_free_conf;
	}
	memset(conf->mirrors, 0, sizeof(struct mirror_info)*mddev->raid_disks);

	conf->r1bio_pool = mempool_create(NR_RAID1_BIOS, r1bio_pool_alloc,
						r1bio_pool_free, mddev);
	if (!conf->r1bio_pool) {
		printk(KERN_ERR "raid1: couldn't allocate memory for %s\n", 
			mdname(mddev));
		goto out_free_conf;
	}
	mddev->queue->unplug_fn = raid1_unplug;


	ITERATE_RDEV(mddev, rdev, tmp) {
		disk_idx = rdev->raid_disk;
		if (disk_idx >= mddev->raid_disks
		    || disk_idx < 0)
			continue;
		disk = conf->mirrors + disk_idx;

		disk->rdev = rdev;

		blk_queue_stack_limits(mddev->queue,
				       rdev->bdev->bd_disk->queue);
		/* as we don't honour merge_bvec_fn, we must never risk
		 * violating it, so limit ->max_sector to one PAGE, as
		 * a one page request is never in violation.
		 */
		if (rdev->bdev->bd_disk->queue->merge_bvec_fn &&
		    mddev->queue->max_sectors > (PAGE_SIZE>>9))
			mddev->queue->max_sectors = (PAGE_SIZE>>9);

		disk->head_position = 0;
		if (!rdev->faulty && rdev->in_sync)
			conf->working_disks++;
	}
	conf->raid_disks = mddev->raid_disks;
	conf->mddev = mddev;
	conf->device_lock = SPIN_LOCK_UNLOCKED;
	if (conf->working_disks == 1)
		mddev->recovery_cp = MaxSector;

	conf->resync_lock = SPIN_LOCK_UNLOCKED;
	init_waitqueue_head(&conf->wait_idle);
	init_waitqueue_head(&conf->wait_resume);

	if (!conf->working_disks) {
		printk(KERN_ERR "raid1: no operational mirrors for %s\n",
			mdname(mddev));
		goto out_free_conf;
	}

	mddev->degraded = 0;
	for (i = 0; i < conf->raid_disks; i++) {

		disk = conf->mirrors + i;

		if (!disk->rdev) {
			disk->head_position = 0;
			mddev->degraded++;
		}
	}

	/*
	 * find the first working one and use it as a starting point
	 * to read balancing.
	 */
	for (j = 0; j < conf->raid_disks &&
		     (!conf->mirrors[j].rdev ||
		      !conf->mirrors[j].rdev->in_sync) ; j++)
		/* nothing */;
	conf->last_used = j;



	{
		mddev->thread = md_register_thread(raid1d, mddev, "%s_raid1");
		if (!mddev->thread) {
			printk(KERN_ERR 
				"raid1: couldn't allocate thread for %s\n", 
				mdname(mddev));
			goto out_free_conf;
		}
	}
	printk(KERN_INFO 
		"raid1: raid set %s active with %d out of %d mirrors\n",
		mdname(mddev), mddev->raid_disks - mddev->degraded, 
		mddev->raid_disks);
	/*
	 * Ok, everything is just fine now
	 */
	mddev->array_size = mddev->size;

	return 0;

out_free_conf:
	if (conf->r1bio_pool)
		mempool_destroy(conf->r1bio_pool);
	if (conf->mirrors)
		kfree(conf->mirrors);
	kfree(conf);
	mddev->private = NULL;
out:
	return -EIO;
}

static int stop(mddev_t *mddev)
{
	conf_t *conf = mddev_to_conf(mddev);

	md_unregister_thread(mddev->thread);
	mddev->thread = NULL;
	if (conf->r1bio_pool)
		mempool_destroy(conf->r1bio_pool);
	if (conf->mirrors)
		kfree(conf->mirrors);
	kfree(conf);
	mddev->private = NULL;
	return 0;
}

static mdk_personality_t raid1_personality =
{
	.name		= "raid1",
	.owner		= THIS_MODULE,
	.make_request	= make_request,
	.run		= run,
	.stop		= stop,
	.status		= status,
	.error_handler	= error,
	.hot_add_disk	= raid1_add_disk,
	.hot_remove_disk= raid1_remove_disk,
	.spare_active	= raid1_spare_active,
	.sync_request	= sync_request,
};

static int __init raid_init(void)
{
	return register_md_personality(RAID1, &raid1_personality);
}

static void raid_exit(void)
{
	unregister_md_personality(RAID1);
}

module_init(raid_init);
module_exit(raid_exit);
MODULE_LICENSE("GPL");
MODULE_ALIAS("md-personality-3"); /* RAID1 */
