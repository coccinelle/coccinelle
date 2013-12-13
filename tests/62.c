static struct usb_driver carl9170_driver = {
	.id_table = carl9170_usb_ids,
	.soft_unbind = 1,
#ifdef CONFIG_PM
	.reset_resume = carl9170_usb_resume,
#endif /* CONFIG_PM */
	.disable_hub_initiated_lpm = 1,
};
