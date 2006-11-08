static void *
probe_scanner(struct usb_device *dev, unsigned int ifnum,
	      const struct usb_device_id *id)
{

	switch (dev->descriptor.idVendor) { /* Scanner specific read timeout parameters */
	case 0x04b8:		/* Seiko/Epson */
		scn->rd_nak_timeout = HZ * 40;
		break;
	case 0x055f:		/* Mustek */
	case 0x0400:		/* Another Mustek */
	case 0x0ff5:		/* And yet another Mustek */
		scn->rd_nak_timeout = HZ * 1;
	default:
		scn->rd_nak_timeout = RD_NAK_TIMEOUT;
	}

}
