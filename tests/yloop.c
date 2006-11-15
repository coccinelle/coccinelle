static int
arxescsi_proc_info(char *buffer, char **start, off_t offset, int length,
		   int host_no, int inout)
{
	struct Scsi_Host *host;

	host = scsi_host_hn_get(host_no);
	if (!host)
		return 0;

	list_for_each_entry(scd, &host->my_devices, siblings) {
	}
	return pos;
}

