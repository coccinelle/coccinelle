static int usb_storage_proc_info (char *buffer, char **start, off_t offset,
		int length, int hostno, int inout)
{
	struct us_data *us;
	char *pos = buffer;
	struct Scsi_Host *hostptr;
	unsigned long f;

	/* if someone is sending us data, just throw it away */
	if (inout)
		return length;

	/* find our data from the given hostno */
	hostptr = scsi_host_hn_get(hostno);
	if (!hostptr) {	 /* if we couldn't find it, we return an error */
		return -ESRCH;
	}
	us = (struct us_data*)hostptr->hostdata[0];

	/* if we couldn't find it, we return an error */
	if (!us) {
//		scsi_host_put(hostptr);
		return -ESRCH;
	}

        // ...

	/* release the reference count on this host */
	scsi_host_put(hostptr);

        // ...

}
