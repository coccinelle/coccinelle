int acornscsi_proc_info(char *buffer, char **start, off_t offset,
			int length, int host_no, int inout)
{
    int pos, begin = 0, devidx;
    struct Scsi_Host *instance;
    Scsi_Device *scd;
    AS_Host *host;
    char *p = buffer;

    instance = scsi_host_hn_get(host_no);

    if (inout == 1 || !instance)
	return -EINVAL;

    host  = (AS_Host *)instance->hostdata;
    
    p += sprintf(p, "AcornSCSI driver v%d.%d.%d"
#ifdef CONFIG_SCSI_ACORNSCSI_SYNC
    " SYNC"
#endif
#ifdef CONFIG_SCSI_ACORNSCSI_TAGGED_QUEUE
    " TAG"
#endif
#ifdef CONFIG_SCSI_ACORNSCSI_LINK
    " LINK"
#endif
#if (DEBUG & DEBUG_NO_WRITE)
    " NOWRITE ("NO_WRITE_STR")"
#endif
		"\n\n", VER_MAJOR, VER_MINOR, VER_PATCH);

    p += sprintf(p,	"SBIC: WD33C93A  Address: %08X  IRQ : %d\n",
			host->scsi.io_port, host->scsi.irq);
#ifdef USE_DMAC
    p += sprintf(p,	"DMAC: uPC71071  Address: %08X  IRQ : %d\n\n",
			host->dma.io_port, host->scsi.irq);
#endif

    p += sprintf(p,	"Statistics:\n"
			"Queued commands: %-10u    Issued commands: %-10u\n"
			"Done commands  : %-10u    Reads          : %-10u\n"
			"Writes         : %-10u    Others         : %-10u\n"
			"Disconnects    : %-10u    Aborts         : %-10u\n"
			"Resets         : %-10u\n\nLast phases:",
			host->stats.queues,		host->stats.removes,
			host->stats.fins,		host->stats.reads,
			host->stats.writes,		host->stats.miscs,
			host->stats.disconnects,	host->stats.aborts,
			host->stats.resets);

    for (devidx = 0; devidx < 9; devidx ++) {
	unsigned int statptr, prev;

	p += sprintf(p, "\n%c:", devidx == 8 ? 'H' : ('0' + devidx));
	statptr = host->status_ptr[devidx] - 10;

	if ((signed int)statptr < 0)
	    statptr += STATUS_BUFFER_SIZE;

	prev = host->status[devidx][statptr].when;

	for (; statptr != host->status_ptr[devidx]; statptr = (statptr + 1) & (STATUS_BUFFER_SIZE - 1)) {
	    if (host->status[devidx][statptr].when) {
		p += sprintf(p, "%c%02X:%02X+%2ld",
			host->status[devidx][statptr].irq ? '-' : ' ',
			host->status[devidx][statptr].ph,
			host->status[devidx][statptr].ssr,
			(host->status[devidx][statptr].when - prev) < 100 ?
				(host->status[devidx][statptr].when - prev) : 99);
		prev = host->status[devidx][statptr].when;
	    }
	}
    }

    p += sprintf(p, "\nAttached devices:\n");

    list_for_each_entry(scd, &instance->my_devices, siblings) {
	p += sprintf(p, "Device/Lun TaggedQ      Sync\n");
	p += sprintf(p, "     %d/%d   ", scd->id, scd->lun);
	if (scd->tagged_supported)
		p += sprintf(p, "%3sabled(%3d) ",
			     scd->tagged_queue ? "en" : "dis",
			     scd->current_tag);
	else
		p += sprintf(p, "unsupported  ");

	if (host->device[scd->id].sync_xfer & 15)
		p += sprintf(p, "offset %d, %d ns\n",
			     host->device[scd->id].sync_xfer & 15,
			     acornscsi_getperiod(host->device[scd->id].sync_xfer));
	else
		p += sprintf(p, "async\n");

	pos = p - buffer;
	if (pos + begin < offset) {
	    begin += pos;
	    p = buffer;
	}
	pos = p - buffer;
	if (pos + begin > offset + length)
	    break;
    }

    pos = p - buffer;

    *start = buffer + (offset - begin);
    pos -= offset - begin;

    if (pos > length)
	pos = length;

    return pos;
}
