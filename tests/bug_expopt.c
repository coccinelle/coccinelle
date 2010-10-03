static int __init xpram_setup_blkdev(void)
{
	unsigned long offset;
	int i, rc = -ENOMEM;

	for (i = 0; i < xpram_devs; i++) {
		struct gendisk *disk = alloc_disk(1);
		if (!disk)
			goto out;
		xpram_disks[i] = disk;
	}

	/*
	 * Register xpram major.
	 */
	rc = register_blkdev(XPRAM_MAJOR, XPRAM_NAME);
	if (rc < 0)
		goto out;

	devfs_mk_dir("slram");

	/*
	 * Assign the other needed values: make request function, sizes and
	 * hardsect size. All the minor devices feature the same value.
	 */
	xpram_queue = blk_alloc_queue(GFP_KERNEL);
	if (!xpram_queue) {
		rc = -ENOMEM;
		goto out_unreg;
	}
	blk_queue_make_request(xpram_queue, xpram_make_request);
	blk_queue_hardsect_size(xpram_queue, 4096);

	/*
	 * Setup device structures.
	 */
	offset = 0;
	for (i = 0; i < xpram_devs; i++) {
		struct gendisk *disk = xpram_disks[i];

		xpram_devices[i].size = xpram_sizes[i] / 4;
		xpram_devices[i].offset = offset;
		offset += xpram_devices[i].size;
		disk->major = XPRAM_MAJOR;
		disk->first_minor = i;
		disk->fops = &xpram_devops;
		disk->private_data = &xpram_devices[i];
		disk->queue = xpram_queue;
		sprintf(disk->disk_name, "slram%d", i);
		sprintf(disk->devfs_name, "slram/%d", i);
		set_capacity(disk, xpram_sizes[i] << 1);
		add_disk(disk);
	}

	return 0;
out_unreg:
	devfs_remove("slram");
	unregister_blkdev(XPRAM_MAJOR, XPRAM_NAME);
out:
	while (i--)
		put_disk(xpram_disks[i]);
	return rc;
}
