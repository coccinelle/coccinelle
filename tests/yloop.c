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

