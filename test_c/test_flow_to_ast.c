/* Probe to see if a new device is actually a SCSI device */
static void * storage_probe(struct usb_device *dev, unsigned int ifnum,
			    const struct usb_device_id *id)
{
	int i;
	const int id_index = id - storage_usb_ids; 
	char mf[USB_STOR_STRING_LEN];		     /* manufacturer */
	char prod[USB_STOR_STRING_LEN];		     /* product */
	char serial[USB_STOR_STRING_LEN];	     /* serial number */
	GUID(guid);			   /* Global Unique Identifier */
	unsigned int flags;
	struct us_unusual_dev *unusual_dev;
	struct us_data *ss = NULL;
#ifdef CONFIG_USB_STORAGE_SDDR09
	int result;
#endif

	/* these are temporary copies -- we test on these, then put them
	 * in the us-data structure 
	 */
	struct usb_endpoint_descriptor *ep_in = NULL;
	struct usb_endpoint_descriptor *ep_out = NULL;
	struct usb_endpoint_descriptor *ep_int = NULL;
	u8 subclass = 0;
	u8 protocol = 0;

	/* the altsettting on the interface we're probing that matched our
	 * usb_match_id table
	 */
	struct usb_interface *intf = dev->actconfig->interface;
	struct usb_interface_descriptor *altsetting =
		intf[ifnum].altsetting + intf[ifnum].act_altsetting;
	US_DEBUGP("act_altsettting is %d\n", intf[ifnum].act_altsetting);

	/* clear the temporary strings */
	memset(mf, 0, sizeof(mf));
	memset(prod, 0, sizeof(prod));
	memset(serial, 0, sizeof(serial));

	/* 
	 * Can we support this device, either because we know about it
	 * from our unusual device list, or because it advertises that it's
	 * compliant to the specification?
	 *
	 * id_index is calculated in the declaration to be the index number
	 * of the match from the usb_device_id table, so we can find the
	 * corresponding entry in the private table.
	 */
	US_DEBUGP("id_index calculated to be: %d\n", id_index);
	US_DEBUGP("Array length appears to be: %d\n", sizeof(us_unusual_dev_list) / sizeof(us_unusual_dev_list[0]));
	if (id_index <
	    sizeof(us_unusual_dev_list) / sizeof(us_unusual_dev_list[0])) {
		unusual_dev = &us_unusual_dev_list[id_index];
		if (unusual_dev->vendorName)
			US_DEBUGP("Vendor: %s\n", unusual_dev->vendorName);
		if (unusual_dev->productName)
			US_DEBUGP("Product: %s\n", unusual_dev->productName);
	} else
		/* no, we can't support it */
		return NULL;

	/* At this point, we know we've got a live one */
	US_DEBUGP("USB Mass Storage device detected\n");

	/* Determine subclass and protocol, or copy from the interface */
	subclass = unusual_dev->useProtocol;
	protocol = unusual_dev->useTransport;
	flags = unusual_dev->flags;

	/*
	 * Find the endpoints we need
	 * We are expecting a minimum of 2 endpoints - in and out (bulk).
	 * An optional interrupt is OK (necessary for CBI protocol).
	 * We will ignore any others.
	 */
	for (i = 0; i < altsetting->bNumEndpoints; i++) {
		/* is it an BULK endpoint? */
		if ((altsetting->endpoint[i].bmAttributes & 
		     USB_ENDPOINT_XFERTYPE_MASK) == USB_ENDPOINT_XFER_BULK) {
			/* BULK in or out? */
			if (altsetting->endpoint[i].bEndpointAddress & 
			    USB_DIR_IN)
				ep_in = &altsetting->endpoint[i];
			else
				ep_out = &altsetting->endpoint[i];
		}

		/* is it an interrupt endpoint? */
		if ((altsetting->endpoint[i].bmAttributes & 
		     USB_ENDPOINT_XFERTYPE_MASK) == USB_ENDPOINT_XFER_INT) {
			ep_int = &altsetting->endpoint[i];
		}
	}
	US_DEBUGP("Endpoints: In: 0x%p Out: 0x%p Int: 0x%p (Period %d)\n",
		  ep_in, ep_out, ep_int, ep_int ? ep_int->bInterval : 0);

#ifdef CONFIG_USB_STORAGE_SDDR09
	if (protocol == US_PR_EUSB_SDDR09 || protocol == US_PR_DPCM_USB) {
		/* set the configuration -- STALL is an acceptable response here */
		result = usb_set_configuration(dev, 1);

		US_DEBUGP("Result from usb_set_configuration is %d\n", result);
		if (result == -EPIPE) {
			US_DEBUGP("-- clearing stall on control interface\n");
			usb_clear_halt(dev, usb_sndctrlpipe(dev, 0));
		} else if (result != 0) {
			/* it's not a stall, but another error -- time to bail */
			US_DEBUGP("-- Unknown error.  Rejecting device\n");
			return NULL;
		}
	}
#endif

	/* Do some basic sanity checks, and bail if we find a problem */
	if (!ep_in || !ep_out || (protocol == US_PR_CBI && !ep_int)) {
		US_DEBUGP("Endpoint sanity check failed! Rejecting dev.\n");
		return NULL;
	}

	/* At this point, we're committed to using the device */
	usb_inc_dev_use(dev);

	/* clear the GUID and fetch the strings */
	GUID_CLEAR(guid);
	if (dev->descriptor.iManufacturer)
		usb_string(dev, dev->descriptor.iManufacturer, 
			   mf, sizeof(mf));
	if (dev->descriptor.iProduct)
		usb_string(dev, dev->descriptor.iProduct, 
			   prod, sizeof(prod));
	if (dev->descriptor.iSerialNumber && !(flags & US_FL_IGNORE_SER))
		usb_string(dev, dev->descriptor.iSerialNumber, 
			   serial, sizeof(serial));

	/* Create a GUID for this device */
	if (dev->descriptor.iSerialNumber && serial[0]) {
		/* If we have a serial number, and it's a non-NULL string */
		make_guid(guid, dev->descriptor.idVendor, 
			  dev->descriptor.idProduct, serial);
	} else {
		/* We don't have a serial number, so we use 0 */
		make_guid(guid, dev->descriptor.idVendor, 
			  dev->descriptor.idProduct, "0");
	}

	/*
	 * Now check if we have seen this GUID before
	 * We're looking for a device with a matching GUID that isn't
	 * already on the system
	 */
	ss = us_list;
	while ((ss != NULL) && 
	       ((ss->pusb_dev) || !GUID_EQUAL(guid, ss->guid)))
		ss = ss->next;

	if (ss != NULL) {
		/* Existing device -- re-connect */
		US_DEBUGP("Found existing GUID " GUID_FORMAT "\n",
			  GUID_ARGS(guid));

		/* lock the device pointers */
		down(&(ss->dev_semaphore));

		/* establish the connection to the new device upon reconnect */
		ss->ifnum = ifnum;
		ss->pusb_dev = dev;

		/* copy over the endpoint data */
		if (ep_in)
			ss->ep_in = ep_in->bEndpointAddress & 
				USB_ENDPOINT_NUMBER_MASK;
		if (ep_out)
			ss->ep_out = ep_out->bEndpointAddress & 
				USB_ENDPOINT_NUMBER_MASK;
		ss->ep_int = ep_int;

		/* allocate an IRQ callback if one is needed */
		if ((ss->protocol == US_PR_CBI) && usb_stor_allocate_irq(ss)) {
			usb_dec_dev_use(dev);
			return NULL;
		}

		/* allocate the URB we're going to use */
		ss->current_urb = usb_alloc_urb(0, GFP_KERNEL);
		if (!ss->current_urb) {
			usb_dec_dev_use(dev);
			return NULL;
		}

                /* Re-Initialize the device if it needs it */
		if (unusual_dev && unusual_dev->initFunction)
			(unusual_dev->initFunction)(ss);

		/* unlock the device pointers */
		up(&(ss->dev_semaphore));

	} else { 
		/* New device -- allocate memory and initialize */
		US_DEBUGP("New GUID " GUID_FORMAT "\n", GUID_ARGS(guid));

		if ((ss = (struct us_data *)kmalloc(sizeof(struct us_data), 
						    GFP_KERNEL)) == NULL) {
			printk(KERN_WARNING USB_STORAGE "Out of memory\n");
			usb_dec_dev_use(dev);
			return NULL;
		}
		memset(ss, 0, sizeof(struct us_data));

		/* allocate the URB we're going to use */
		ss->current_urb = usb_alloc_urb(0, GFP_KERNEL);
		if (!ss->current_urb) {
			kfree(ss);
			usb_dec_dev_use(dev);
			return NULL;
		}

		/* Initialize the mutexes only when the struct is new */
		init_completion(&(ss->notify));
		init_MUTEX_LOCKED(&(ss->ip_waitq));
		init_MUTEX(&(ss->queue_exclusion));
		init_MUTEX(&(ss->irq_urb_sem));
		init_MUTEX(&(ss->current_urb_sem));
		init_MUTEX(&(ss->dev_semaphore));

		/* copy over the subclass and protocol data */
		ss->subclass = subclass;
		ss->protocol = protocol;
		ss->flags = flags;
		ss->unusual_dev = unusual_dev;

		/* copy over the endpoint data */
		if (ep_in)
			ss->ep_in = ep_in->bEndpointAddress & 
				USB_ENDPOINT_NUMBER_MASK;
		if (ep_out)
			ss->ep_out = ep_out->bEndpointAddress & 
				USB_ENDPOINT_NUMBER_MASK;
		ss->ep_int = ep_int;

		/* establish the connection to the new device */
		ss->ifnum = ifnum;
		ss->pusb_dev = dev;

		/* copy over the identifiying strings */
		strncpy(ss->vendor, mf, USB_STOR_STRING_LEN);
		strncpy(ss->product, prod, USB_STOR_STRING_LEN);
		strncpy(ss->serial, serial, USB_STOR_STRING_LEN);
		if (strlen(ss->vendor) == 0) {
			if (unusual_dev->vendorName)
				strncpy(ss->vendor, unusual_dev->vendorName,
					USB_STOR_STRING_LEN);
			else
				strncpy(ss->vendor, "Unknown",
					USB_STOR_STRING_LEN);
		}
		if (strlen(ss->product) == 0) {
			if (unusual_dev->productName)
				strncpy(ss->product, unusual_dev->productName,
					USB_STOR_STRING_LEN);
			else
				strncpy(ss->product, "Unknown",
					USB_STOR_STRING_LEN);
		}
		if (strlen(ss->serial) == 0)
			strncpy(ss->serial, "None", USB_STOR_STRING_LEN);

		/* copy the GUID we created before */
		memcpy(ss->guid, guid, sizeof(guid));

		/* 
		 * Set the handler pointers based on the protocol
		 * Again, this data is persistant across reattachments
		 */
		switch (ss->protocol) {
		case US_PR_CB:
			ss->transport_name = "Control/Bulk";
			ss->transport = usb_stor_CB_transport;
			ss->transport_reset = usb_stor_CB_reset;
			ss->max_lun = 7;
			break;

		case US_PR_CBI:
			ss->transport_name = "Control/Bulk/Interrupt";
			ss->transport = usb_stor_CBI_transport;
			ss->transport_reset = usb_stor_CB_reset;
			ss->max_lun = 7;
			break;

		case US_PR_BULK:
			ss->transport_name = "Bulk";
			ss->transport = usb_stor_Bulk_transport;
			ss->transport_reset = usb_stor_Bulk_reset;
			ss->max_lun = usb_stor_Bulk_max_lun(ss);
			break;

#ifdef CONFIG_USB_STORAGE_HP8200e
		case US_PR_SCM_ATAPI:
			ss->transport_name = "SCM/ATAPI";
			ss->transport = hp8200e_transport;
			ss->transport_reset = usb_stor_CB_reset;
			ss->max_lun = 1;
			break;
#endif

#ifdef CONFIG_USB_STORAGE_SDDR09
		case US_PR_EUSB_SDDR09:
			ss->transport_name = "EUSB/SDDR09";
			ss->transport = sddr09_transport;
			ss->transport_reset = usb_stor_CB_reset;
			ss->max_lun = 0;
			break;
#endif

#ifdef CONFIG_USB_STORAGE_DPCM
		case US_PR_DPCM_USB:
			ss->transport_name = "Control/Bulk-EUSB/SDDR09";
			ss->transport = dpcm_transport;
			ss->transport_reset = usb_stor_CB_reset;
			ss->max_lun = 1;
			break;
#endif

#ifdef CONFIG_USB_STORAGE_FREECOM
                case US_PR_FREECOM:
                        ss->transport_name = "Freecom";
                        ss->transport = freecom_transport;
                        ss->transport_reset = usb_stor_freecom_reset;
                        ss->max_lun = 0;
                        break;
#endif

#ifdef CONFIG_USB_STORAGE_DATAFAB
                case US_PR_DATAFAB:
                        ss->transport_name  = "Datafab Bulk-Only";
                        ss->transport = datafab_transport;
                        ss->transport_reset = usb_stor_Bulk_reset;
                        ss->max_lun = 1;
                        break;
#endif

#ifdef CONFIG_USB_STORAGE_JUMPSHOT
                case US_PR_JUMPSHOT:
                        ss->transport_name  = "Lexar Jumpshot Control/Bulk";
                        ss->transport = jumpshot_transport;
                        ss->transport_reset = usb_stor_Bulk_reset;
                        ss->max_lun = 1;
                        break;
#endif

		default:
			ss->transport_name = "Unknown";
			kfree(ss->current_urb);
			kfree(ss);
			usb_dec_dev_use(dev);
			return NULL;
			break;
		}
		US_DEBUGP("Transport: %s\n", ss->transport_name);

		/* fix for single-lun devices */
		if (ss->flags & US_FL_SINGLE_LUN)
			ss->max_lun = 0;

		switch (ss->subclass) {
		case US_SC_RBC:
			ss->protocol_name = "Reduced Block Commands (RBC)";
			ss->proto_handler = usb_stor_transparent_scsi_command;
			break;

		case US_SC_8020:
			ss->protocol_name = "8020i";
			ss->proto_handler = usb_stor_ATAPI_command;
			ss->max_lun = 0;
			break;

		case US_SC_QIC:
			ss->protocol_name = "QIC-157";
			ss->proto_handler = usb_stor_qic157_command;
			ss->max_lun = 0;
			break;

		case US_SC_8070:
			ss->protocol_name = "8070i";
			ss->proto_handler = usb_stor_ATAPI_command;
			ss->max_lun = 0;
			break;

		case US_SC_SCSI:
			ss->protocol_name = "Transparent SCSI";
			ss->proto_handler = usb_stor_transparent_scsi_command;
			break;

		case US_SC_UFI:
			ss->protocol_name = "Uniform Floppy Interface (UFI)";
			ss->proto_handler = usb_stor_ufi_command;
			break;

#ifdef CONFIG_USB_STORAGE_ISD200
                case US_SC_ISD200:
                        ss->protocol_name = "ISD200 ATA/ATAPI";
                        ss->proto_handler = isd200_ata_command;
                        break;
#endif

		default:
			ss->protocol_name = "Unknown";
			kfree(ss->current_urb);
			kfree(ss);
			return NULL;
			break;
		}
		US_DEBUGP("Protocol: %s\n", ss->protocol_name);

		/* allocate an IRQ callback if one is needed */
		if ((ss->protocol == US_PR_CBI) && usb_stor_allocate_irq(ss)) {
			usb_dec_dev_use(dev);
			return NULL;
		}

		/*
		 * Since this is a new device, we need to generate a scsi 
		 * host definition, and register with the higher SCSI layers
		 */

		/* Initialize the host template based on the default one */
		memcpy(&(ss->htmplt), &usb_stor_host_template, 
		       sizeof(usb_stor_host_template));

		/* Grab the next host number */
		ss->host_number = my_host_number++;

		/* We abuse this pointer so we can pass the ss pointer to 
		 * the host controller thread in us_detect.  But how else are
		 * we to do it?
		 */
		(struct us_data *)ss->htmplt.proc_dir = ss; 

		/* Just before we start our control thread, initialize
		 * the device if it needs initialization */
		if (unusual_dev && unusual_dev->initFunction)
			unusual_dev->initFunction(ss);

		/* start up our control thread */
		ss->pid = kernel_thread(usb_stor_control_thread, ss,
					CLONE_VM);
		if (ss->pid < 0) {
			printk(KERN_WARNING USB_STORAGE 
			       "Unable to start control thread\n");
			kfree(ss->current_urb);
			kfree(ss);
			usb_dec_dev_use(dev);
			return NULL;
		}

		/* wait for the thread to start */
		wait_for_completion(&(ss->notify));

		/* now register	 - our detect function will be called */
		ss->htmplt.module = THIS_MODULE;
		scsi_register_host(&ss->htmplt);

		/* lock access to the data structures */
		down(&us_list_semaphore);

		/* put us in the list */
		ss->next = us_list;
		us_list = ss;

		/* release the data structure lock */
		up(&us_list_semaphore);
	}

	printk(KERN_DEBUG 
	       "WARNING: USB Mass Storage data integrity not assured\n");
	printk(KERN_DEBUG 
	       "USB Mass Storage device found at %d\n", dev->devnum);

	/* return a pointer for the disconnect function */
	return ss;
}
