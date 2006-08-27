static int nsp32_proc_info(char  *buffer,
			   char **start,
			   off_t  offset,
			   int    length,
			   int    hostno,
			   int    inout)
{
	char *pos = buffer;
	int thislength;
	unsigned long flags;
	nsp32_hw_data *data;
	struct Scsi_Host *host = NULL;
	unsigned int base;
	unsigned char mode_reg;

	/* Write is not supported, just return. */
	if (inout == TRUE) {
		return -EINVAL;
	}

	/* search this HBA host */
	
	host = scsi_host_hn_get(hostno);
	
	if (host == NULL) {
		return -ESRCH;
	}
	data = (nsp32_hw_data *)host->hostdata;
	base = host->io_port;

	SPRINTF("NinjaSCSI-32 status\n\n");
	SPRINTF("Driver version:        %s\n",		nsp32_release_version);
	SPRINTF("SCSI host No.:         %d\n",		hostno);
	SPRINTF("IRQ:                   %d\n",		host->irq);
	SPRINTF("IO:                    0x%lx-0x%lx\n", host->io_port, host->io_port + host->n_io_port - 1);
	SPRINTF("MMIO(virtual address): 0x%lx\n",	host->base);
	SPRINTF("sg_tablesize:          %d\n",		host->sg_tablesize);
	SPRINTF("Chip revision:         %d\n",		(nsp32_read2(base, INDEX_REG) >> 8) - 0x4f);

	mode_reg = nsp32_index_read1(base, CHIP_MODE);

#ifdef CONFIG_PM
	//SPRINTF("Power Management:      %s\n",          (mode_reg & OPTF) ? "yes" : "no");
#endif
	SPRINTF("OEM:                   %s\n",          nsp32_model[mode_reg & (OEM0|OEM1)]);

	spin_lock_irqsave(&(data->Lock), flags);
	SPRINTF("CurrentSC:             0x%p\n\n",      data->CurrentSC);
	spin_unlock_irqrestore(&(data->Lock), flags);

	thislength = pos - (buffer + offset);

	if(thislength < 0) {
		*start = 0;
                return 0;
        }


	thislength = MIN(thislength, length);
	*start = buffer + offset;

	return thislength;
}
