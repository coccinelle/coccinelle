static int hidg_setup(struct usb_function *f,
		const struct usb_ctrlrequest *ctrl)
{
	VDBG(cdev, "hid_setup crtl_request : bRequestType:0x%x bRequest:0x%x "
		"Value:0x%x\n", ctrl->bRequestType, ctrl->bRequest, value);
}
