static int __init sn9c102_module_init(void)
{
	int err = 0;

	KDBG(2, SN9C102_MODULE_NAME " v" SN9C102_MODULE_VERSION)
	KDBG(3, SN9C102_MODULE_AUTHOR)

	if ((err = usb_register(&sn9c102_usb_driver)))
		KDBG(1, "usb_register() failed")

 	return err;
}
