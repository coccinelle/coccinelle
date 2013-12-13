static struct usb_driver carl9170_driver = {
	.id_table = carl9170_usb_ids,
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,27))
	.soft_unbind = 1,
#endif
#ifdef CONFIG_PM
	.reset_resume = carl9170_usb_resume,
#endif /* CONFIG_PM */
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(3,5,0))
	.disable_hub_initiated_lpm = 1,
#endif
};
