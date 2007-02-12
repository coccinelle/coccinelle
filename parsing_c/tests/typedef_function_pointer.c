struct ep_attribute {
	struct attribute attr;
 	ssize_t (*show)(struct usb_device *,
			struct usb_endpoint_descriptor *, char *);
};
