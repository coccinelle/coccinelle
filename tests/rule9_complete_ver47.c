static int sym53c8xx_proc_info(char *buffer, char **start, off_t offset,
			int length, int hostno, int func)
{
	struct Scsi_Host *host;
	struct host_data *host_data;
	hcb_p np = 0;
	int retv;

	host = scsi_host_hn_get(hostno);
	if (!host)
		return -EINVAL;

	host_data = (struct host_data *) host->hostdata;
	np = host_data->ncb;
	if (!np)
		return -EINVAL;

	if (func) {
#ifdef	SYM_LINUX_USER_COMMAND_SUPPORT
		retv = sym_user_command(np, buffer, length);
#else
		retv = -EINVAL;
#endif
	}
	else {
		if (start)
			*start = buffer;
#ifdef SYM_LINUX_USER_INFO_SUPPORT
		retv = sym_host_info(np, buffer, offset, length);
#else
		retv = -EINVAL;
#endif
	}

	scsi_host_put(host);
	return retv;
}
