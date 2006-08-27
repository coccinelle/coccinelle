int generic_NCR5380_proc_info(char *buffer, char **start, off_t offset, int length, int hostno, int inout)
{
	int len = 0;
	NCR5380_local_declare();
	unsigned long flags;
	unsigned char status;
	int i;
	struct Scsi_Host *scsi_ptr;
	Scsi_Cmnd *ptr;
	struct NCR5380_hostdata *hostdata;
#ifdef NCR5380_STATS
	Scsi_Device *dev;
	extern const char *const scsi_device_types[MAX_SCSI_DEVICE_CODE];
#endif

	/* For now this is constant so we may walk it */
	scsi_ptr = scsi_host_hn_get(hostno);
	
	NCR5380_setup(scsi_ptr);
	hostdata = (struct NCR5380_hostdata *) scsi_ptr->hostdata;

	spin_lock_irqsave(scsi_ptr->host_lock, flags);
	PRINTP("SCSI host number %d : %s\n" ANDP scsi_ptr->host_no ANDP scsi_ptr->hostt->name);
	PRINTP("Generic NCR5380 driver version %d\n" ANDP GENERIC_NCR5380_PUBLIC_RELEASE);
	PRINTP("NCR5380 core version %d\n" ANDP NCR5380_PUBLIC_RELEASE);
#ifdef NCR53C400
	PRINTP("NCR53C400 extension version %d\n" ANDP NCR53C400_PUBLIC_RELEASE);
	PRINTP("NCR53C400 card%s detected\n" ANDP(((struct NCR5380_hostdata *) scsi_ptr->hostdata)->flags & FLAG_NCR53C400) ? "" : " not");
# if NCR53C400_PSEUDO_DMA
	PRINTP("NCR53C400 pseudo DMA used\n");
# endif
#else
	PRINTP("NO NCR53C400 driver extensions\n");
#endif
	PRINTP("Using %s mapping at %s 0x%lx, " ANDP STRVAL(NCR5380_map_config) ANDP STRVAL(NCR5380_map_name) ANDP scsi_ptr->NCR5380_instance_name);
	if (scsi_ptr->irq == SCSI_IRQ_NONE)
		PRINTP("no interrupt\n");
	else
		PRINTP("on interrupt %d\n" ANDP scsi_ptr->irq);

#ifdef NCR5380_STATS
	if (hostdata->connected || hostdata->issue_queue || hostdata->disconnected_queue)
		PRINTP("There are commands pending, transfer rates may be crud\n");
	if (hostdata->pendingr)
		PRINTP("  %d pending reads" ANDP hostdata->pendingr);
	if (hostdata->pendingw)
		PRINTP("  %d pending writes" ANDP hostdata->pendingw);
	if (hostdata->pendingr || hostdata->pendingw)
		PRINTP("\n");
	list_for_each_entry (dev, &scsi_ptr->my_devices, siblings) {
		unsigned long br = hostdata->bytes_read[dev->id];
		unsigned long bw = hostdata->bytes_write[dev->id];
		long tr = hostdata->time_read[dev->id] / HZ;
		long tw = hostdata->time_write[dev->id] / HZ;

		PRINTP("  T:%d %s " ANDP dev->id ANDP(dev->type < MAX_SCSI_DEVICE_CODE) ? scsi_device_types[(int) dev->type] : "Unknown");
		for (i = 0; i < 8; i++)
			if (dev->vendor[i] >= 0x20)
				*(buffer + (len++)) = dev->vendor[i];
		*(buffer + (len++)) = ' ';
		for (i = 0; i < 16; i++)
			if (dev->model[i] >= 0x20)
				*(buffer + (len++)) = dev->model[i];
		*(buffer + (len++)) = ' ';
		for (i = 0; i < 4; i++)
			if (dev->rev[i] >= 0x20)
				*(buffer + (len++)) = dev->rev[i];
		*(buffer + (len++)) = ' ';

		PRINTP("\n%10ld kb read    in %5ld secs" ANDP br / 1024 ANDP tr);
		if (tr)
			PRINTP(" @ %5ld bps" ANDP br / tr);

		PRINTP("\n%10ld kb written in %5ld secs" ANDP bw / 1024 ANDP tw);
		if (tw)
			PRINTP(" @ %5ld bps" ANDP bw / tw);
		PRINTP("\n");
	}
#endif

	status = NCR5380_read(STATUS_REG);
	if (!(status & SR_REQ))
		PRINTP("REQ not asserted, phase unknown.\n");
	else {
		for (i = 0; (phases[i].value != PHASE_UNKNOWN) && (phases[i].value != (status & PHASE_MASK)); ++i);
		PRINTP("Phase %s\n" ANDP phases[i].name);
	}

	if (!hostdata->connected) {
		PRINTP("No currently connected command\n");
	} else {
		len += sprint_Scsi_Cmnd(buffer, len, (Scsi_Cmnd *) hostdata->connected);
	}

	PRINTP("issue_queue\n");

	for (ptr = (Scsi_Cmnd *) hostdata->issue_queue; ptr; ptr = (Scsi_Cmnd *) ptr->host_scribble)
		len += sprint_Scsi_Cmnd(buffer, len, ptr);

	PRINTP("disconnected_queue\n");

	for (ptr = (Scsi_Cmnd *) hostdata->disconnected_queue; ptr; ptr = (Scsi_Cmnd *) ptr->host_scribble)
		len += sprint_Scsi_Cmnd(buffer, len, ptr);

	*start = buffer + offset;
	len -= offset;
	if (len > length)
		len = length;
	spin_unlock_irqrestore(scsi_ptr->host_lock, flags);
	return len;
}
