int acsi_init( void )
{
	int err = 0;
	int i, target, lun;
	struct acsi_info_struct *aip;
#ifdef CONFIG_ATARI_SLM
	int n_slm = 0;
#endif
	if (!MACH_IS_ATARI || !ATARIHW_PRESENT(ACSI))
		return 0;
	if (register_blkdev(ACSI_MAJOR, "ad")) {
		err = -EBUSY;
		goto out1;
	}
	if (!(acsi_buffer =
		  (char *)atari_stram_alloc(ACSI_BUFFER_SIZE, "acsi"))) {
		err = -ENOMEM;
		printk( KERN_ERR "Unable to get ACSI ST-Ram buffer.\n" );
		goto out2;
	}
	phys_acsi_buffer = virt_to_phys( acsi_buffer );
	STramMask = ATARIHW_PRESENT(EXTD_DMA) ? 0x00000000 : 0xff000000;
	
	acsi_queue = blk_init_queue(do_acsi_request, &acsi_lock);
	if (!acsi_queue) {
		err = -ENOMEM;
		goto out2a;
	}
#ifdef CONFIG_ATARI_SLM
	err = slm_init();
#endif
	if (err)
		goto out3;

	printk( KERN_INFO "Probing ACSI devices:\n" );
	NDevices = 0;
#ifdef CONFIG_ATARI_SLM_MODULE
	for( i = 0; i < 8; ++i )
		SLM_devices[i] = -1;
#endif
	stdma_lock(NULL, NULL);

	for (target = 0; target < 8 && NDevices < MAX_DEV; ++target) {
		lun = 0;
		do {
			aip = &acsi_info[NDevices];
			aip->type = NONE;
			aip->target = target;
			aip->lun = lun;
			i = acsi_devinit(aip);
			switch (i) {
			  case DEV_SUPPORTED:
				printk( KERN_INFO "Detected ");
				switch (aip->type) {
				  case HARDDISK:
					printk("disk");
					break;
				  case CDROM:
					printk("cdrom");
					break;
				  default:
				}
				printk(" ad%c at id %d lun %d ",
				       'a' + NDevices, target, lun);
				if (aip->removable) 
					printk("(removable) ");
				if (aip->read_only) 
					printk("(read-only) ");
				if (aip->size == DEFAULT_SIZE)
					printk(" unkown size, using default ");
				printk("%ld MByte\n",
				       (aip->size*512+1024*1024/2)/(1024*1024));
				NDevices++;
				break;
			  case DEV_SLM:
#ifdef CONFIG_ATARI_SLM
				n_slm += attach_slm( target, lun );
				break;
#endif
#ifdef CONFIG_ATARI_SLM_MODULE
				SLM_devices[target] = lun;
				break;
#endif
				/* neither of the above: fall through to unknown device */
			  case DEV_UNKNOWN:
				printk( KERN_INFO "Detected unsupported device at "
						"id %d lun %d\n", target, lun);
				break;
			}
		}
#ifdef CONFIG_ACSI_MULTI_LUN
		while (i != DEV_NONE && ++lun < MAX_LUN);
#else
		while (0);
#endif
	}

	/* reenable interrupt */
	ENABLE_IRQ();
	stdma_release();

#ifndef CONFIG_ATARI_SLM
	printk( KERN_INFO "Found %d ACSI device(s) total.\n", NDevices );
#else
	printk( KERN_INFO "Found %d ACSI device(s) and %d SLM printer(s) total.\n",
			NDevices, n_slm );
#endif
	err = -ENOMEM;
	for( i = 0; i < NDevices; ++i ) {
		acsi_gendisk[i] = alloc_disk(16);
		if (!acsi_gendisk[i])
			goto out4;
	}

	for( i = 0; i < NDevices; ++i ) {
		struct gendisk *disk = acsi_gendisk[i];
		sprintf(disk->disk_name, "ad%c", 'a'+i);
		aip = &acsi_info[NDevices];
		sprintf(disk->devfs_name, "ad/target%d/lun%d", aip->target, aip->lun);
		disk->major = ACSI_MAJOR;
		disk->first_minor = i << 4;
		if (acsi_info[i].type != HARDDISK) {
			disk->minors = 1;
			strcat(disk->devfs_name, "/disc");
		}
		disk->fops = &acsi_fops;
		disk->private_data = &acsi_info[i];
		set_capacity(disk, acsi_info[i].size);
		disk->queue = acsi_queue;
		add_disk(disk);
	}
	return 0;
out4:
	while (i--)
		put_disk(acsi_gendisk[i]);
out3:
	blk_cleanup_queue(acsi_queue);
out2a:
	atari_stram_free( acsi_buffer );
out2:
	unregister_blkdev( ACSI_MAJOR, "ad" );
out1:
	return err;
}
