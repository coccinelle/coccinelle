static int usb_storage_proc_info (struct Scsi_Host *hostptr, char *buffer, char **start, off_t offset,
		int length, int inout)
{
	struct us_data *us;
	char *pos = buffer;
	unsigned long f;

	/* if someone is sending us data, just throw it away */
	if (inout)
		return length;

	us = (struct us_data*)hostptr->hostdata[0];

	/* if we couldn't find it, we return an error */
	if (!us) {
		return -ESRCH;
	}

        // ...


        // ...

}
