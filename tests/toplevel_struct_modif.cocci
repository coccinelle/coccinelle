@@
identifier proc_info_func, usb_stor_host_template;
@@

struct SHT usb_stor_host_template = {
	.name =				"usb-storage",
	.proc_name =			"usb-storage",
-	.proc_info =			proc_info_func,
	.ioctl =			NULL,
};
