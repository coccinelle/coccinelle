struct SHT usb_stor_host_template = {
	/* basic userland interface stuff */
	.name =				"usb-storage",
	.proc_name =			"usb-storage",
	.proc_info =			usb_storage_proc_info,
	.proc_dir =			NULL,
	.info =				usb_storage_info,
	.ioctl =			NULL,

	/* old-style detect and release */
	.detect =			NULL,
	.release =			NULL,

	/* command interface -- queued only */
	.command =			NULL,
	.queuecommand =			usb_storage_queuecommand,

	/* error and abort handlers */
	.eh_abort_handler =		usb_storage_command_abort,
	.eh_device_reset_handler =	usb_storage_device_reset,
	.eh_bus_reset_handler =		usb_storage_bus_reset,
	.eh_host_reset_handler =	NULL,
	.eh_strategy_handler =		NULL,

	/* queue commands only, only one command per LUN */
	.can_queue =			1,
	.cmd_per_lun =			1,

	/* unknown initiator id */
	.this_id =			-1,

	/* no limit on commands */
	.max_sectors =			0,
	
	/* pre- and post- device scan functions */
	.slave_alloc =			NULL,
	.slave_configure =		NULL,
	.slave_destroy =		NULL,

	/* lots of sg segments can be handled */
	.sg_tablesize =			SG_ALL,

	/* use 32-bit address space for DMA */
	.unchecked_isa_dma =		FALSE,
	.highmem_io =			FALSE,

	/* merge commands... this seems to help performance, but
	 * periodically someone should test to see which setting is more
	 * optimal.
	 */
	.use_clustering =		TRUE,

	/* emulated HBA */
	.emulated =			TRUE,

	/* sorry, no BIOS to help us */
	.bios_param =			NULL,

	/* module management */
	.module =			THIS_MODULE
};

/* For a device that is "Not Ready" */
unsigned char usb_stor_sense_notready[18] = {
	[0]	= 0x70,			    /* current error */
	[2]	= 0x02,			    /* not ready */
	[7]	= 0x0a,			    /* additional length */
	[12]	= 0x04,			    /* not ready */
	[13]	= 0x03			    /* manual intervention */
};
