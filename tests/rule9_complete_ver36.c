int eesoxscsi_proc_info(char *buffer, char **start, off_t offset,
			    int length, int host_no, int inout)
{
	struct Scsi_Host *host;
	struct eesoxscsi_info *info;
	char *p = buffer;
	int pos;

	host = scsi_host_hn_get(host_no);
	if (!host)
		return 0;

	if (inout == 1)
		return eesoxscsi_set_proc_info(host, buffer, length);

	info = (struct eesoxscsi_info *)host->hostdata;

	p += sprintf(p, "EESOX SCSI driver v%s\n", VERSION);
	p += fas216_print_host(&info->info, p);
	p += sprintf(p, "Term    : o%s\n",
			info->control & EESOX_TERM_ENABLE ? "n" : "ff");

	pos += fas216_print_stats(&info->info, buffer + pos);
	p += fas216_print_stats(&info->info, p);
	p += fas216_print_devices(&info->info, p);

	if (pos > length)
		pos = length;

	return pos;
}
