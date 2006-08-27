int fcal_proc_info (char *buffer, char **start, off_t offset, int length, int hostno, int inout)
{
	struct Scsi_Host *host = NULL;
	struct fcal *fcal;
	fc_channel *fc;
	char *pos = buffer;
	int i, j;

	host = scsi_host_hn_get(hostno);

	if (!host) return -ESRCH;

	if (inout) return length;
    
	fcal = (struct fcal *)host->hostdata;
	fc = fcal->fc;

#ifdef __sparc__
	SPRINTF ("Sun Enterprise Network Array (A5000 or E?500) on %s PROM node %x\n", fc->name, fc->dev->prom_node);
#else
	SPRINTF ("Fibre Channel Arbitrated Loop on %s\n", fc->name);
#endif
	SPRINTF ("Initiator AL-PA: %02x\n", fc->sid);

	SPRINTF ("\nAttached devices: %s\n", !list_empty(&host->my_devices) ? "" : "none");
	
	for (i = 0; i < fc->posmap->len; i++) {
		unsigned char alpa = fc->posmap->list[i];
		unsigned char target;
		u32 *u1, *u2;
		
		target = alpa2target[alpa];
		u1 = (u32 *)&fcal->nport_wwn[target];
		u2 = (u32 *)&fcal->node_wwn[target];
		if (!u1[0] && !u1[1]) {
			SPRINTF ("  [AL-PA: %02x] Not responded to PLOGI\n", alpa);
		} else if (!fcal->map[target]) {
			SPRINTF ("  [AL-PA: %02x, Port WWN: %08x%08x, Node WWN: %08x%08x] Not responded to PRLI\n",
				 alpa, u1[0], u1[1], u2[0], u2[1]);
		} else {
			Scsi_Device *scd;
			list_for_each_entry (scd, &host->my_devices, siblings)
				if (scd->id == target) {
					SPRINTF ("  [AL-PA: %02x, Id: %02d, Port WWN: %08x%08x, Node WWN: %08x%08x]  ",
						alpa, target, u1[0], u1[1], u2[0], u2[1]);
					SPRINTF ("%s ", (scd->type < MAX_SCSI_DEVICE_CODE) ?
						scsi_device_types[(short) scd->type] : "Unknown device");

					for (j = 0; (j < 8) && (scd->vendor[j] >= 0x20); j++)
						SPRINTF ("%c", scd->vendor[j]);
					SPRINTF (" ");

					for (j = 0; (j < 16) && (scd->model[j] >= 0x20); j++)
						SPRINTF ("%c", scd->model[j]);
		
					SPRINTF ("\n");
				}
		}
	}
	SPRINTF ("\n");

	*start = buffer + offset;

	if ((pos - buffer) < offset)
		return 0;
	else if (pos - buffer - offset < length)
		return pos - buffer - offset;
	else
		return length;
}
