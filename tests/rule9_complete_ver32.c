STATIC int
NCR_700_proc_directory_info(char *proc_buf, char **startp,
			 off_t offset, int bytes_available,
			 int host_no, int write)
{
	static char buf[4096];	/* 1 page should be sufficient */
	int len = 0;
	struct Scsi_Host *host;
	struct NCR_700_Host_Parameters *hostdata;
	Scsi_Device *SDp;

	host = scsi_host_hn_get(host_no);
	if(host == NULL)
		return 0;

	if(write) {
		/* FIXME: Clear internal statistics here */
		return 0;
	}
	hostdata = (struct NCR_700_Host_Parameters *)host->hostdata[0];
	len += sprintf(&buf[len], "Total commands outstanding: %d\n", hostdata->command_slot_count);
	len += sprintf(&buf[len],"\
Target	Active  Next Tag\n\
======	======  ========\n");
	list_for_each_entry(SDp, &host->my_devices, siblings) {
		len += sprintf(&buf[len]," %2d:%2d   %4d      %4d\n", SDp->id, SDp->lun, NCR_700_get_depth(SDp), SDp->current_tag);
	}
	if((len -= offset) <= 0)
		return 0;
	if(len > bytes_available)
		len = bytes_available;
	memcpy(proc_buf, buf + offset, len);
	return len;
}
