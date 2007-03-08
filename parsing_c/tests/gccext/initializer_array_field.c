static struct block_device_operations gscd_fops = {

  .ep[0] = {}
};




static struct lh7a40x_udc memory = {
	.usb_address = 0,

	.gadget = {
		   .ops = &lh7a40x_udc_ops,
		   .ep0 = &memory.ep[0].ep,
		   .name = driver_name,
		   .dev = {
			   .bus_id = "gadget",
			   .release = nop_release,
			   },
		   },

	/* control endpoint */
 	.ep[0] = {
		  .ep = {
			 .name = ep0name,
			 .ops = &lh7a40x_ep_ops,
			 .maxpacket = EP0_PACKETSIZE,
			 },
		  .dev = &memory,

		  .bEndpointAddress = 0,
		  .bmAttributes = 0,

		  .ep_type = ep_control,
		  .fifo = io_p2v(USB_EP0_FIFO),
		  .csr1 = USB_EP0_CSR,
		  .csr2 = USB_EP0_CSR,
		  },

	/* first group of endpoints */
	.ep[1] = {
		  .ep = {
			 .name = "ep1in-bulk",
			 .ops = &lh7a40x_ep_ops,
			 .maxpacket = 64,
			 },
		  .dev = &memory,

		  .bEndpointAddress = USB_DIR_IN | 1,
		  .bmAttributes = USB_ENDPOINT_XFER_BULK,

		  .ep_type = ep_bulk_in,
		  .fifo = io_p2v(USB_EP1_FIFO),
		  .csr1 = USB_IN_CSR1,
		  .csr2 = USB_IN_CSR2,
		  },

	.ep[2] = {
		  .ep = {
			 .name = "ep2out-bulk",
			 .ops = &lh7a40x_ep_ops,
			 .maxpacket = 64,
			 },
		  .dev = &memory,

		  .bEndpointAddress = 2,
		  .bmAttributes = USB_ENDPOINT_XFER_BULK,

		  .ep_type = ep_bulk_out,
		  .fifo = io_p2v(USB_EP2_FIFO),
		  .csr1 = USB_OUT_CSR1,
		  .csr2 = USB_OUT_CSR2,
		  },

	.ep[3] = {
		  .ep = {
			 .name = "ep3in-int",
			 .ops = &lh7a40x_ep_ops,
			 .maxpacket = 64,
			 },
		  .dev = &memory,

		  .bEndpointAddress = USB_DIR_IN | 3,
		  .bmAttributes = USB_ENDPOINT_XFER_INT,

		  .ep_type = ep_interrupt,
		  .fifo = io_p2v(USB_EP3_FIFO),
		  .csr1 = USB_IN_CSR1,
		  .csr2 = USB_IN_CSR2,
		  },
};
