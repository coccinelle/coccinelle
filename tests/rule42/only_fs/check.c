/*
 *  fs/partitions/check.c
 *
 *  Code extracted from drivers/block/genhd.c
 *  Copyright (C) 1991-1998  Linus Torvalds
 *  Re-organised Feb 1998 Russell King
 *
 *  We now have independent partition support from the
 *  block drivers, which allows all the partition code to
 *  be grouped in one location, and it to be mostly self
 *  contained.
 *
 *  Added needed MAJORS for new pairs, {hdi,hdj}, {hdk,hdl}
 */

#include <linux/config.h>
#include <linux/fs.h>
#include <linux/genhd.h>
#include <linux/kernel.h>
#include <linux/major.h>
#include <linux/blk.h>
#include <linux/init.h>
#include <linux/raid/md.h>

#include "check.h"

#include "acorn.h"
#include "amiga.h"
#include "atari.h"
#include "ldm.h"
#include "mac.h"
#include "msdos.h"
#include "osf.h"
#include "sgi.h"
#include "sun.h"
#include "ibm.h"
#include "ultrix.h"
#include "efi.h"

int warn_no_part = 1; /*This is ugly: should make genhd removable media aware*/

static int (*check_part[])(struct gendisk *hd, struct block_device *bdev,
			   unsigned long first_sect, int first_minor) = {
#ifdef CONFIG_ACORN_PARTITION
	acorn_partition,
#endif
#ifdef CONFIG_EFI_PARTITION
	efi_partition,		/* this must come before msdos */
#endif
#ifdef CONFIG_LDM_PARTITION
	ldm_partition,		/* this must come before msdos */
#endif
#ifdef CONFIG_MSDOS_PARTITION
	msdos_partition,
#endif
#ifdef CONFIG_OSF_PARTITION
	osf_partition,
#endif
#ifdef CONFIG_SUN_PARTITION
	sun_partition,
#endif
#ifdef CONFIG_AMIGA_PARTITION
	amiga_partition,
#endif
#ifdef CONFIG_ATARI_PARTITION
	atari_partition,
#endif
#ifdef CONFIG_MAC_PARTITION
	mac_partition,
#endif
#ifdef CONFIG_SGI_PARTITION
	sgi_partition,
#endif
#ifdef CONFIG_ULTRIX_PARTITION
	ultrix_partition,
#endif
#ifdef CONFIG_IBM_PARTITION
	ibm_partition,
#endif
	NULL
};

/*
 *	This is ucking fugly but its probably the best thing for 2.4.x
 *	Take it as a clear reminder than we should put the device name
 *	generation in the object kdev_t points to in 2.5.
 */
 
#ifdef CONFIG_ARCH_S390
int (*genhd_dasd_name)(char*,int,int,struct gendisk*) = NULL;
EXPORT_SYMBOL(genhd_dasd_name);
#endif

/*
 * disk_name() is used by partition check code and the md driver.
 * It formats the devicename of the indicated disk into
 * the supplied buffer (of size at least 32), and returns
 * a pointer to that same buffer (for convenience).
 */

char *disk_name (struct gendisk *hd, int minor, char *buf)
{
	const char *maj = hd->major_name;
	unsigned int unit = (minor >> hd->minor_shift);
	unsigned int part = (minor & ((1 << hd->minor_shift) -1 ));

	if ((unit < hd->nr_real) && hd->part[minor].de) {
		int pos;

		pos = devfs_generate_path (hd->part[minor].de, buf, 64);
		if (pos >= 0)
			return buf + pos;
	}

#ifdef CONFIG_ARCH_S390
	if (genhd_dasd_name
	    && genhd_dasd_name (buf, unit, part, hd) == 0)
		return buf;
#endif
	/*
	 * IDE devices use multiple major numbers, but the drives
	 * are named as:  {hda,hdb}, {hdc,hdd}, {hde,hdf}, {hdg,hdh}..
	 * This requires special handling here.
	 */
	switch (hd->major) {
		case IDE9_MAJOR:
			unit += 2;
		case IDE8_MAJOR:
			unit += 2;
		case IDE7_MAJOR:
			unit += 2;
		case IDE6_MAJOR:
			unit += 2;
		case IDE5_MAJOR:
			unit += 2;
		case IDE4_MAJOR:
			unit += 2;
		case IDE3_MAJOR:
			unit += 2;
		case IDE2_MAJOR:
			unit += 2;
		case IDE1_MAJOR:
			unit += 2;
		case IDE0_MAJOR:
			maj = "hd";
			break;
		case MD_MAJOR:
			sprintf(buf, "%s%d", maj, unit);
			return buf;
	}
	if (hd->major >= SCSI_DISK1_MAJOR && hd->major <= SCSI_DISK7_MAJOR) {
		unit = unit + (hd->major - SCSI_DISK1_MAJOR + 1) * 16;
		if (unit+'a' > 'z') {
			unit -= 26;
			sprintf(buf, "sd%c%c", 'a' + unit / 26, 'a' + unit % 26);
			if (part)
				sprintf(buf + 4, "%d", part);
			return buf;
		}
	}
	if (hd->major >= COMPAQ_SMART2_MAJOR && hd->major <= COMPAQ_SMART2_MAJOR+7) {
		int ctlr = hd->major - COMPAQ_SMART2_MAJOR;
 		if (part == 0)
 			sprintf(buf, "%s/c%dd%d", maj, ctlr, unit);
 		else
 			sprintf(buf, "%s/c%dd%dp%d", maj, ctlr, unit, part);
 		return buf;
 	}
	if (hd->major >= COMPAQ_CISS_MAJOR && hd->major <= COMPAQ_CISS_MAJOR+7) {
                int ctlr = hd->major - COMPAQ_CISS_MAJOR;
                if (part == 0)
                        sprintf(buf, "%s/c%dd%d", maj, ctlr, unit);
                else
                        sprintf(buf, "%s/c%dd%dp%d", maj, ctlr, unit, part);
                return buf;
	}
	if (hd->major >= DAC960_MAJOR && hd->major <= DAC960_MAJOR+7) {
		int ctlr = hd->major - DAC960_MAJOR;
 		if (part == 0)
 			sprintf(buf, "%s/c%dd%d", maj, ctlr, unit);
 		else
 			sprintf(buf, "%s/c%dd%dp%d", maj, ctlr, unit, part);
 		return buf;
 	}
	if (hd->major == ATARAID_MAJOR) {
		int disk = minor >> hd->minor_shift;
		int part = minor & (( 1 << hd->minor_shift) - 1);
		if (part == 0)
			sprintf(buf, "%s/d%d", maj, disk);
		else
			sprintf(buf, "%s/d%dp%d", maj, disk, part);
		return buf;
	}
	if (part)
		sprintf(buf, "%s%c%d", maj, unit+'a', part);
	else
		sprintf(buf, "%s%c", maj, unit+'a');
	return buf;
}

/*
 * Add a partitions details to the devices partition description.
 */
void add_gd_partition(struct gendisk *hd, int minor, int start, int size)
{
#ifndef CONFIG_DEVFS_FS
	char buf[40];
#endif

	hd->part[minor].start_sect = start;
	hd->part[minor].nr_sects   = size;
#ifdef CONFIG_DEVFS_FS
	printk(" p%d", (minor & ((1 << hd->minor_shift) - 1)));
#else
	if ((hd->major >= COMPAQ_SMART2_MAJOR+0 && hd->major <= COMPAQ_SMART2_MAJOR+7) ||
	    (hd->major >= COMPAQ_CISS_MAJOR+0 && hd->major <= COMPAQ_CISS_MAJOR+7))
		printk(" p%d", (minor & ((1 << hd->minor_shift) - 1)));
	else
		printk(" %s", disk_name(hd, minor, buf));
#endif
}

static void check_partition(struct gendisk *hd, kdev_t dev, int first_part_minor)
{
	devfs_handle_t de = NULL;
	static int first_time = 1;
	unsigned long first_sector;
	struct block_device *bdev;
	char buf[64];
	int i;

	if (first_time)
		printk(KERN_INFO "Partition check:\n");
	first_time = 0;
	first_sector = hd->part[minor(dev)].start_sect;

	/*
	 * This is a kludge to allow the partition check to be
	 * skipped for specific drives (e.g. IDE CD-ROM drives)
	 */
	if ((int)first_sector == -1) {
		hd->part[minor(dev)].start_sect = 0;
		return;
	}

	if (hd->de_arr)
		de = hd->de_arr[minor(dev) >> hd->minor_shift];
	i = devfs_generate_path (de, buf, sizeof buf);
	if (i >= 0)
		printk(KERN_INFO " /dev/%s:", buf + i);
	else
		printk(KERN_INFO " %s:", disk_name(hd, minor(dev), buf));
	bdev = bdget(kdev_t_to_nr(dev));
	bdev->bd_contains = bdev;
	bdev->bd_inode->i_size = (loff_t)hd->part[minor(dev)].nr_sects << 9;
	bdev->bd_inode->i_blkbits = blksize_bits(block_size(dev));
	for (i = 0; check_part[i]; i++) {
		int res;
		res = check_part[i](hd, bdev, first_sector, first_part_minor);
		if (res) {
			if (res < 0 &&  warn_no_part)
				printk(" unable to read partition table\n");
			goto setup_devfs;
		}
	}

	printk(" unknown partition table\n");
setup_devfs:
	invalidate_bdev(bdev, 1);
	truncate_inode_pages(bdev->bd_inode->i_mapping, 0);
	bdput(bdev);
	i = first_part_minor - 1;
	devfs_register_partitions (hd, i, hd->sizes ? 0 : 1);
}

#ifdef CONFIG_DEVFS_FS
static void devfs_register_partition (struct gendisk *dev, int minor, int part)
{
	int devnum = minor >> dev->minor_shift;
	devfs_handle_t dir;
	unsigned int devfs_flags = DEVFS_FL_DEFAULT;
	char devname[16];

	if (dev->part[minor + part].de) return;
	dir = devfs_get_parent (dev->part[minor].de);
	if (!dir) return;
	if ( dev->flags && (dev->flags[devnum] & GENHD_FL_REMOVABLE) )
		devfs_flags |= DEVFS_FL_REMOVABLE;
	sprintf (devname, "part%d", part);
	dev->part[minor + part].de =
	    devfs_register (dir, devname, devfs_flags,
			    dev->major, minor + part,
			    S_IFBLK | S_IRUSR | S_IWUSR,
			    dev->fops, NULL);
}

static struct unique_numspace disc_numspace = UNIQUE_NUMBERSPACE_INITIALISER;

static void devfs_register_disc (struct gendisk *dev, int minor)
{
	int pos = 0;
	int devnum = minor >> dev->minor_shift;
	devfs_handle_t dir, slave;
	unsigned int devfs_flags = DEVFS_FL_DEFAULT;
	char dirname[64], symlink[16];
	static devfs_handle_t devfs_handle;

	if (dev->part[minor].de) return;
	if ( dev->flags && (dev->flags[devnum] & GENHD_FL_REMOVABLE) )
		devfs_flags |= DEVFS_FL_REMOVABLE;
	if (dev->de_arr) {
		dir = dev->de_arr[devnum];
		if (!dir)  /*  Aware driver wants to block disc management  */
			return;
		pos = devfs_generate_path (dir, dirname + 3, sizeof dirname-3);
		if (pos < 0) return;
		strncpy (dirname + pos, "../", 3);
	}
	else {
		/*  Unaware driver: construct "real" directory  */
		sprintf (dirname, "../%s/disc%d", dev->major_name, devnum);
		dir = devfs_mk_dir (NULL, dirname + 3, NULL);
	}
	if (!devfs_handle)
		devfs_handle = devfs_mk_dir (NULL, "discs", NULL);
	dev->part[minor].number = devfs_alloc_unique_number (&disc_numspace);
	sprintf (symlink, "disc%d", dev->part[minor].number);
	devfs_mk_symlink (devfs_handle, symlink, DEVFS_FL_DEFAULT,
			  dirname + pos, &slave, NULL);
	dev->part[minor].de =
	    devfs_register (dir, "disc", devfs_flags, dev->major, minor,
			    S_IFBLK | S_IRUSR | S_IWUSR, dev->fops, NULL);
	devfs_auto_unregister (dev->part[minor].de, slave);
	if (!dev->de_arr)
		devfs_auto_unregister (slave, dir);
}
#endif  /*  CONFIG_DEVFS_FS  */

void devfs_register_partitions (struct gendisk *dev, int minor, int unregister)
{
#ifdef CONFIG_DEVFS_FS
	int part, max_p;

	if (!unregister)
		devfs_register_disc (dev, minor);
	max_p = (1 << dev->minor_shift);
	for (part = 1; part < max_p; part++) {
		if ( unregister || (dev->part[part + minor].nr_sects < 1) ) {
			devfs_unregister (dev->part[part + minor].de);
			dev->part[part + minor].de = NULL;
			continue;
		}
		devfs_register_partition (dev, minor, part);
	}
	if (unregister) {
		devfs_unregister (dev->part[minor].de);
		dev->part[minor].de = NULL;
		devfs_dealloc_unique_number (&disc_numspace,
					     dev->part[minor].number);
	}
#endif  /*  CONFIG_DEVFS_FS  */
}

/*
 * This function will re-read the partition tables for a given device,
 * and set things back up again.  There are some important caveats,
 * however.  You must ensure that no one is using the device, and no one
 * can start using the device while this function is being executed.
 *
 * Much of the cleanup from the old partition tables should have already been
 * done
 */

void register_disk(struct gendisk *gdev, kdev_t dev, unsigned minors,
	struct block_device_operations *ops, long size)
{
	if (!gdev)
		return;
	grok_partitions(dev, size);
}

void grok_partitions(kdev_t dev, long size)
{
	int i, minors, first_minor, end_minor;
	struct gendisk *g = get_gendisk(dev);

	if (!g)
		return;

	minors = 1 << g->minor_shift;
	first_minor = minor(dev);
	if (first_minor & (minors-1)) {
		printk("grok_partitions: bad device 0x%02x:%02x\n",
		       major(dev), first_minor);
		first_minor &= ~(minors-1);
	}
	end_minor = first_minor + minors;
 
	if (!g->sizes)
		blk_size[g->major] = NULL;

	g->part[first_minor].nr_sects = size;

	/* No such device or no minors to use for partitions */
	if (!size || minors == 1)
		return;

	if (g->sizes) {
		g->sizes[first_minor] = size >> (BLOCK_SIZE_BITS - 9);
		for (i = first_minor + 1; i < end_minor; i++)
			g->sizes[i] = 0;
	}
	blk_size[g->major] = g->sizes;
	check_partition(g, mk_kdev(g->major, first_minor), 1 + first_minor);

 	/*
 	 * We need to set the sizes array before we will be able to access
 	 * any of the partitions on this device.
 	 */
	if (g->sizes != NULL) {	/* optional safeguard in ll_rw_blk.c */
		for (i = first_minor; i < end_minor; i++)
			g->sizes[i] = g->part[i].nr_sects >> (BLOCK_SIZE_BITS - 9);
	}
}

unsigned char *read_dev_sector(struct block_device *bdev, unsigned long n, Sector *p)
{
	struct address_space *mapping = bdev->bd_inode->i_mapping;
	int sect = PAGE_CACHE_SIZE / 512;
	struct page *page;

	page = read_cache_page(mapping, n/sect,
			(filler_t *)mapping->a_ops->readpage, NULL);
	if (!IS_ERR(page)) {
		wait_on_page(page);
		if (!Page_Uptodate(page))
			goto fail;
		if (PageError(page))
			goto fail;
		p->v = page;
		return (unsigned char *)page_address(page) + 512 * (n % sect);
fail:
		page_cache_release(page);
	}
	p->v = NULL;
	return NULL;
}

int wipe_partitions(kdev_t dev)
{
	struct gendisk *g;
	kdev_t devp;
	int p, major, minor, minor0, max_p, res;

	g = get_gendisk(dev);
	if (g == NULL)
		return -EINVAL;

	max_p = 1 << g->minor_shift;
	major = major(dev);
	minor = minor(dev);
	minor0 = minor & ~(max_p - 1);
	if (minor0 != minor)		/* for now only whole-disk reread */
		return -EINVAL;		/* %%% later.. */

	/* invalidate stuff */
	for (p = max_p - 1; p >= 0; p--) {
		minor = minor0 + p;
		devp = mk_kdev(major,minor);
#if 0					/* %%% superfluous? */
		if (g->part[minor].nr_sects == 0)
			continue;
#endif
		res = invalidate_device(devp, 1);
		if (res)
			return res;
		g->part[minor].start_sect = 0;
		g->part[minor].nr_sects = 0;
	}

	/* some places do blksize_size[major][minor] = 1024,
	   as preparation for reading partition table - superfluous */
	/* sd.c used to set blksize_size to 2048 in case
	   rscsi_disks[target].device->sector_size == 2048 */

	return 0;
}

/*
 * Make sure that a proposed subpartition is strictly contained inside
 * the parent partition.  If all is well, call add_gd_partition().
 */
int
check_and_add_subpartition(struct gendisk *hd, int super_minor, int minor,
			   int sub_start, int sub_size)
{
	int start = hd->part[super_minor].start_sect;
	int size = hd->part[super_minor].nr_sects;

	if (start == sub_start && size == sub_size) {
		/* full parent partition, we have it already */
		return 0;
	}

	if (start <= sub_start && start+size >= sub_start+sub_size) {
		add_gd_partition(hd, minor, sub_start, sub_size);
		return 1;
	}

	printk("bad subpartition - ignored\n");
	return 0;
}
