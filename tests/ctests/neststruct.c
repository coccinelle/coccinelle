struct usb_hub {
	union {
		struct usb_hub_status	hub;
		struct usb_port_status	port;
	}			*status;	/* buffer for status reports */

};

static int hub_configure(struct usb_hub *hub)
{
	hub->status = kmalloc(sizeof(*hub->status), GFP_KERNEL);

}
