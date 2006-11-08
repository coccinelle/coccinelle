static void *
probe_scanner(struct usb_device *dev, unsigned int ifnum,
	      const struct usb_device_id *id)
{
	struct scn_usb_data *scn;
	struct usb_interface_descriptor *interface;
	struct usb_endpoint_descriptor *endpoint;

	int ep_cnt;
	int ix;
	int scn_minor;

	char valid_device = 0;
	char have_bulk_in, have_bulk_out, have_intr;
	char name[10];

	if (vendor != -1 && product != -1) {
		info("probe_scanner: User specified USB scanner -- Vendor:Product - %x:%x", vendor, product);
	}

	dbg("probe_scanner: USB dev address:%p", dev);
	dbg("probe_scanner: ifnum:%u", ifnum);

/*
 * 1. Check Vendor/Product
 * 2. Determine/Assign Bulk Endpoints
 * 3. Determine/Assign Intr Endpoint
 */

/*
 * There doesn't seem to be an imaging class defined in the USB
 * Spec. (yet).  If there is, HP isn't following it and it doesn't
 * look like anybody else is either.  Therefore, we have to test the
 * Vendor and Product ID's to see what we have.  Also, other scanners
 * may be able to use this driver by specifying both vendor and
 * product ID's as options to the scanner module in conf.modules.
 *
 * NOTE: Just because a product is supported here does not mean that
 * applications exist that support the product.  It's in the hopes
 * that this will allow developers a means to produce applications
 * that will support USB products.
 *
 * Until we detect a device which is pleasing, we silently punt.
 */

	for (ix = 0; ix < sizeof (scanner_device_ids) / sizeof (struct usb_device_id); ix++) {
		if ((dev->descriptor.idVendor == scanner_device_ids [ix].idVendor) &&
		    (dev->descriptor.idProduct == scanner_device_ids [ix].idProduct)) {
			valid_device = 1;
			break;
                }
	}
	if (dev->descriptor.idVendor == vendor &&   /* User specified */
	    dev->descriptor.idProduct == product) { /* User specified */
		valid_device = 1;
	}

        if (!valid_device)
                return NULL;    /* We didn't find anything pleasing */

/*
 * After this point we can be a little noisy about what we are trying to
 *  configure.
 */

	if (dev->descriptor.bNumConfigurations != 1) {
		info("probe_scanner: Only one device configuration is supported.");
		return NULL;
	}

	if (dev->config[0].bNumInterfaces != 1) {
		info("probe_scanner: Only one device interface is supported.");
		return NULL;
	}

	interface = dev->config[0].interface[ifnum].altsetting;
	endpoint = interface[ifnum].endpoint;

/*
 * Start checking for two bulk endpoints OR two bulk endpoints *and* one
 * interrupt endpoint. If we have an interrupt endpoint go ahead and
 * setup the handler. FIXME: This is a future enhancement...
 */

	dbg("probe_scanner: Number of Endpoints:%d", (int) interface->bNumEndpoints);

	if ((interface->bNumEndpoints != 2) && (interface->bNumEndpoints != 3)) {
		info("probe_scanner: Only two or three endpoints supported.");
		return NULL;
	}

	ep_cnt = have_bulk_in = have_bulk_out = have_intr = 0;

	while (ep_cnt < interface->bNumEndpoints) {

		if (!have_bulk_in && IS_EP_BULK_IN(endpoint[ep_cnt])) {
			ep_cnt++;
			have_bulk_in = ep_cnt;
			dbg("probe_scanner: bulk_in_ep:%d", have_bulk_in);
			continue;
		}

		if (!have_bulk_out && IS_EP_BULK_OUT(endpoint[ep_cnt])) {
			ep_cnt++;
			have_bulk_out = ep_cnt;
			dbg("probe_scanner: bulk_out_ep:%d", have_bulk_out);
			continue;
		}

		if (!have_intr && IS_EP_INTR(endpoint[ep_cnt])) {
			ep_cnt++;
			have_intr = ep_cnt;
			dbg("probe_scanner: intr_ep:%d", have_intr);
			continue;
		}
		info("probe_scanner: Undetected endpoint -- consult Documentation/usb/scanner.txt.");
		return NULL;	/* Shouldn't ever get here unless we have something weird */
	}


/*
 * Perform a quick check to make sure that everything worked as it
 * should have.
 */

	switch(interface->bNumEndpoints) {
	case 2:
		if (!have_bulk_in || !have_bulk_out) {
			info("probe_scanner: Two bulk endpoints required.");
			return NULL;
		}
		break;
	case 3:
		if (!have_bulk_in || !have_bulk_out || !have_intr) {
			info("probe_scanner: Two bulk endpoints and one interrupt endpoint required.");
			return NULL;
		}
		break;
	default:
		info("probe_scanner: Endpoint determination failed --  consult Documentation/usb/scanner.txt");
		return NULL;
	}


/*
 * Determine a minor number and initialize the structure associated
 * with it.  The problem with this is that we are counting on the fact
 * that the user will sequentially add device nodes for the scanner
 * devices.  */
	
	down(&scn_mutex);

	for (scn_minor = 0; scn_minor < SCN_MAX_MNR; scn_minor++) {
		if (!p_scn_table[scn_minor])
			break;
	}

/* Check to make sure that the last slot isn't already taken */
	if (p_scn_table[scn_minor]) {
		err("probe_scanner: No more minor devices remaining.");
		up(&scn_mutex);
		return NULL;
	}

	dbg("probe_scanner: Allocated minor:%d", scn_minor);

	if (!(scn = kmalloc (sizeof (struct scn_usb_data), GFP_KERNEL))) {
		err("probe_scanner: Out of memory.");
		up(&scn_mutex);
		return NULL;
	}
	memset (scn, 0, sizeof(struct scn_usb_data));

	scn->scn_irq = usb_alloc_urb(0);
	if (!scn->scn_irq) {
		kfree(scn);
		up(&scn_mutex);
		return NULL;
	}

	init_MUTEX(&(scn->sem)); /* Initializes to unlocked */

	dbg ("probe_scanner(%d): Address of scn:%p", scn_minor, scn);

/* Ok, if we detected an interrupt EP, setup a handler for it */
	if (have_intr) {
		dbg("probe_scanner(%d): Configuring IRQ handler for intr EP:%d", scn_minor, have_intr);
		FILL_INT_URB(scn->scn_irq, dev,
			     usb_rcvintpipe(dev, have_intr),
			     &scn->button, 1, irq_scanner, scn,
			     // endpoint[(int)have_intr].bInterval);
			     250);

	        if (usb_submit_urb(scn->scn_irq)) {
			err("probe_scanner(%d): Unable to allocate INT URB.", scn_minor);
                	kfree(scn);
			up(&scn_mutex);
                	return NULL;
        	}
	}


/* Ok, now initialize all the relevant values */
	if (!(scn->obuf = (char *)kmalloc(OBUF_SIZE, GFP_KERNEL))) {
		err("probe_scanner(%d): Not enough memory for the output buffer.", scn_minor);
		kfree(scn);
		up(&scn_mutex);
		return NULL;
	}
	dbg("probe_scanner(%d): obuf address:%p", scn_minor, scn->obuf);

	if (!(scn->ibuf = (char *)kmalloc(IBUF_SIZE, GFP_KERNEL))) {
		err("probe_scanner(%d): Not enough memory for the input buffer.", scn_minor);
		kfree(scn->obuf);
		kfree(scn);
		up(&scn_mutex);
		return NULL;
	}
	dbg("probe_scanner(%d): ibuf address:%p", scn_minor, scn->ibuf);
	

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


	if (read_timeout > 0) {	/* User specified read timeout overrides everything */
		info("probe_scanner: User specified USB read timeout - %d", read_timeout);
		scn->rd_nak_timeout = read_timeout;
	}


	scn->bulk_in_ep = have_bulk_in;
	scn->bulk_out_ep = have_bulk_out;
	scn->intr_ep = have_intr;
	scn->present = 1;
	scn->scn_dev = dev;
	scn->scn_minor = scn_minor;
	scn->isopen = 0;

	sprintf(name, "scanner%d", scn->scn_minor);
	
	scn->devfs = devfs_register(usb_devfs_handle, name,
				    DEVFS_FL_DEFAULT, USB_MAJOR,
				    SCN_BASE_MNR + scn->scn_minor,
				    S_IFCHR | S_IRUSR | S_IWUSR | S_IRGRP |
				    S_IWGRP | S_IROTH | S_IWOTH, &usb_scanner_fops, NULL);
	if (scn->devfs == NULL)
		dbg("scanner%d: device node registration failed", scn_minor);

	p_scn_table[scn_minor] = scn;

	up(&scn_mutex);

	return scn;
}
