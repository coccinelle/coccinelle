static int
acornscsi_sbic_wait(AS_Host *host, int stat_mask, int stat, int timeout, char *msg)
{
	int asr;

	do {
		asr = sbic_arm_read(host->scsi.io_port, ASR);

		if ((asr & stat_mask) == stat)
			return 0;

		udelay(1);
	} while (--timeout);

	printk("scsi%d: timeout while %s\n", host->host->host_no, msg);

	return -1;
}
