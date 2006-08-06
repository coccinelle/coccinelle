static int usb_storage_proc_info (char *buffer, char **start, off_t offset,
		int length, int hostno, int inout)
{
	struct us_data *us;
	char *pos = buffer;
	struct Scsi_Host *hostptr;
	unsigned long f;

	if (inout)
		return length;

	hostptr = scsi_host_hn_get(hostno);

	if (!hostptr) {
		return -ESRCH;
	}
	us = (struct us_data*)hostptr->hostdata[0];

	scsi_host_put(hostptr);
}
