static int sym53c8xx_proc_info(char *buffer, char **start, off_t offset,
			int length, int hostno, int func)
{
	struct Scsi_Host *host;
	struct host_data *host_data;
	ncb_p ncb = 0;
	int retv;

#ifdef DEBUG_PROC_INFO
printk("sym53c8xx_proc_info: hostno=%d, func=%d\n", hostno, func);
#endif

	host = scsi_host_hn_get(hostno);
	if (!host)
		return -EINVAL;

	host_data = (struct host_data *) host->hostdata;
	ncb = host_data->ncb;
	retv = -EINVAL;
	if (!ncb)
		goto out;

	if (func) {
#ifdef	SCSI_NCR_USER_COMMAND_SUPPORT
		retv = ncr_user_command(ncb, buffer, length);
#endif
	} else {
		if (start)
			*start = buffer;
#ifdef SCSI_NCR_USER_INFO_SUPPORT
		retv = ncr_host_info(ncb, buffer, offset, length);
#endif
	}

out:
	scsi_host_put(host);
	return retv;
}
