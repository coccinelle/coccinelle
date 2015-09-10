typedef struct {
	struct work_struct ppa_tq;	/* Polling interrupt stuff       */
} ppa_struct;

static void ppa_interrupt(void *data)
{
	ppa_struct *dev = (ppa_struct *) data;
	schedule_delayed_work(&dev->ppa_tq, 1);
}

static int ppa_queuecommand(struct scsi_cmnd *cmd,
		void (*done) (struct scsi_cmnd *))
{
	ppa_struct *dev = ppa_dev(cmd->device->host);
	schedule_delayed_work(&dev->ppa_tq, 0);
}
