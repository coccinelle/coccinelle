static int nsp_proc_info(char  *buffer,
			 char **start,
			 off_t  offset,
			 int    length,
			 int    hostno,
			 int    inout)
{
	int id;
	char *pos = buffer;
	int thislength;
	int speed;
	unsigned long flags;
	nsp_hw_data *data = &nsp_data;
	struct Scsi_Host *host = NULL;

	if (inout) {
		return -EINVAL;
	}

	/* search this HBA host */
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(2,5,45))
	host = scsi_host_hn_get(hostno);
#else
	for (host=scsi_hostlist; host; host=host->next) {
                if (host->host_no == hostno) {
                        break;
                }
        }
#endif
	if (host == NULL) {
		return -ESRCH;
	}

	SPRINTF("NinjaSCSI status\n\n");
	SPRINTF("Driver version:        $Revision$\n");
	SPRINTF("SCSI host No.:         %d\n",          hostno);
	SPRINTF("IRQ:                   %d\n",          host->irq);
	SPRINTF("IO:                    0x%lx-0x%lx\n", host->io_port, host->io_port + host->n_io_port - 1);
	SPRINTF("MMIO(virtual address): 0x%lx\n",       host->base);
	SPRINTF("sg_tablesize:          %d\n",          host->sg_tablesize);

	SPRINTF("burst transfer mode:   ");
	switch (nsp_burst_mode) {
	case BURST_IO8:
		SPRINTF("io8");
		break;
	case BURST_IO32:
		SPRINTF("io32");
		break;
	case BURST_MEM32:
		SPRINTF("mem32");
		break;
	default:
		SPRINTF("???");
		break;
	}
	SPRINTF("\n");


	spin_lock_irqsave(&(data->Lock), flags);
	SPRINTF("CurrentSC:             0x%p\n\n",      data->CurrentSC);
	spin_unlock_irqrestore(&(data->Lock), flags);

	SPRINTF("SDTR status\n");
	for(id = 0; id < N_TARGET; id++) {

		SPRINTF("id %d: ", id);

		if (id == host->this_id) {
			SPRINTF("----- NinjaSCSI-3 host adapter\n");
			continue;
		}

		switch(data->Sync[id].SyncNegotiation) {
		case SYNC_OK:
			SPRINTF(" sync");
			break;
		case SYNC_NG:
			SPRINTF("async");
			break;
		case SYNC_NOT_YET:
			SPRINTF(" none");
			break;
		default:
			SPRINTF("?????");
			break;
		}

		if (data->Sync[id].SyncPeriod != 0) {
			speed = 1000000 / (data->Sync[id].SyncPeriod * 4);

			SPRINTF(" transfer %d.%dMB/s, offset %d",
				speed / 1000,
				speed % 1000,
				data->Sync[id].SyncOffset
				);
		}
		SPRINTF("\n");
	}

	thislength = pos - (buffer + offset);

	if(thislength < 0) {
		*start = 0;
                return 0;
        }


	thislength = MIN(thislength, length);
	*start = buffer + offset;

	return thislength;
}
