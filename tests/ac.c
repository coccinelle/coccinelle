static Scsi_Host_Template acornscsi_template = {
	.module			= THIS_MODULE,
	.proc_info		= acornscsi_proc_info,
	.name			= "AcornSCSI",
	.info			= acornscsi_info,
	.queuecommand		= acornscsi_queuecmd,
#warning fixme
	.abort			= acornscsi_abort,
	.reset			= acornscsi_reset,
	.can_queue		= 16,
	.this_id		= 7,
	.sg_tablesize		= SG_ALL,
	.cmd_per_lun		= 2,
	.unchecked_isa_dma	= 0,
	.use_clustering		= DISABLE_CLUSTERING,
	.proc_name		= "acornscsi",
};
