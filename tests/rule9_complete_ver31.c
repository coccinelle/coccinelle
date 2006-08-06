static int sbp2scsi_proc_info(char *buffer, char **start, off_t offset,
			      int length, int hostno, int inout)
{
	Scsi_Device *scd;
	struct Scsi_Host *scsi_host;
	struct hpsb_host *host;
	char *pos = buffer;

	/* if someone is sending us data, just throw it away */
	if (inout)
		return length;

	scsi_host = scsi_host_hn_get(hostno);
	if (!scsi_host)  /* if we couldn't find it, we return an error */
		return -ESRCH;

	host = hpsb_get_host_bykey(&sbp2_highlevel, (unsigned long)scsi_host);
	if (!host) /* shouldn't happen, but... */
		return -ESRCH;

	SPRINTF("Host scsi%d             : SBP-2 IEEE-1394 (%s)\n", hostno,
		host->driver->name);
	SPRINTF("Driver version         : %s\n", version);

	SPRINTF("\nModule options         :\n");
	SPRINTF("  max_speed            : %s\n", hpsb_speedto_str[max_speed]);
	SPRINTF("  max_sectors          : %d\n", max_sectors);
	SPRINTF("  serialize_io         : %s\n", serialize_io ? "yes" : "no");
	SPRINTF("  exclusive_login      : %s\n", exclusive_login ? "yes" : "no");

	SPRINTF("\nAttached devices       : %s\n", !list_empty(&scsi_host->my_devices) ?
		"" : "none");

	list_for_each_entry (scd, &scsi_host->my_devices, siblings) {
		int i;

		SPRINTF("  [Channel: %02d, Id: %02d, Lun: %02d]  ", scd->channel,
			scd->id, scd->lun);
		SPRINTF("%s ", (scd->type < MAX_SCSI_DEVICE_CODE) ?
			scsi_device_types[(short) scd->type] : "Unknown device");

		for (i = 0; (i < 8) && (scd->vendor[i] >= 0x20); i++)
			SPRINTF("%c", scd->vendor[i]);

		SPRINTF(" ");

		for (i = 0; (i < 16) && (scd->model[i] >= 0x20); i++)
			SPRINTF("%c", scd->model[i]);

		SPRINTF("\n");
	}

	SPRINTF("\n");

	/* release the reference count on this host */
	scsi_host_put(scsi_host);
	/* Calculate start of next buffer, and return value. */
	*start = buffer + offset;

	if ((pos - buffer) < offset)
		return (0);
	else if ((pos - buffer - offset) < length)
		return (pos - buffer - offset);
	else
		return (length);
}
