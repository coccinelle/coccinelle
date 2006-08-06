int cumanascsi_2_proc_info (char *buffer, char **start, off_t offset,
			    int length, int host_no, int inout)
{
	struct Scsi_Host *host;
	struct cumanascsi2_info *info;
	char *p = buffer;
	int pos;

	host = scsi_host_hn_get(host_no);
	if (!host)
		return 0;

	if (inout == 1)
		return cumanascsi_2_set_proc_info(host, buffer, length);

	info = (struct cumanascsi2_info *)host->hostdata;

	p += sprintf(p, "Cumana SCSI II driver v%s\n", VERSION);
	p += fas216_print_host(&info->info, p);
	p += sprintf(p, "Term    : o%s\n",
			info->terms ? "n" : "ff");

	p += fas216_print_stats(&info->info, p);
	p += fas216_print_devices(&info->info, p);

	*start = buffer + offset;
	pos = p - buffer - offset;
	if (pos > length)
		pos = length;

	return pos;
}
