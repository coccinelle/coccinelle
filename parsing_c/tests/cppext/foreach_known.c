static int
arxescsi_proc_info(char *buffer)
{
	host = scsi_host_hn_get(hostno);
	if (!host)
		return 0;

 	list_for_each_entry(scd, &host->my_devices, siblings) {
	}
	return pos;
}


// for_all_sbusdev(sdev, sbus) {

static int
arxescsi_proc_info(char *buffer)
{

	ITERATE_RDEV(mddev, rdev, tmp)
		if (rdev->in_sync && !rdev->faulty)
			md_super_write(mddev, rdev,
				       (rdev->sb_offset<<1) + offset
				       + page->index * (PAGE_SIZE/512),
				       PAGE_SIZE,
				       page);

 	pci_for_each_dev(dev)
		acpi_pci_irq_enable(dev);


}



static int
arxescsi_proc_info(char *buffer)
{

	ITERATE_RDEV(mddev,rdev,tmp)
		if (kdev_same(dev_unit(rdev->dev), dev_unit(dev)))
			return rdev;


		ITERATE_MDDEV(mddev,tmp)
			do_md_stop (mddev, 1);


	list_for_each(tmp, &info->tx_urbs_free)
		room++;


        do_each_task_pid(tty->session, PIDTYPE_SID, p) {
        }


	list_for_each(l, &sclp_vt220_empty)
		count += SCLP_VT220_MAX_CHARS_PER_BUFFER;
}
