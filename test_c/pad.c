
static int usb_storage_proc_info (char *buffer, char **start, off_t offset,
		int length, int hostno, int inout)
{
	struct us_data *us;
	char *pos = buffer;
	struct Scsi_Host *hostptr;
	unsigned long f;

	/* if someone is sending us data, just throw it away */
	if (inout)
		return length;

	/* find our data from the given hostno */
	hostptr = scsi_host_hn_get(hostno);
	if (!hostptr) {	 /* if we couldn't find it, we return an error */
		return -ESRCH;
	}
	us = (struct us_data*)hostptr->hostdata[0];

	/* if we couldn't find it, we return an error */
	if (!us) {
		scsi_host_put(hostptr);
		return -ESRCH;
	}

	/* print the controller name */
	SPRINTF("   Host scsi%d: usb-storage\n", hostno);

	/* print product, vendor, and serial number strings */
	SPRINTF("       Vendor: %s\n", us->vendor);
	SPRINTF("      Product: %s\n", us->product);
	SPRINTF("Serial Number: %s\n", us->serial);

	/* show the protocol and transport */
	SPRINTF("     Protocol: %s\n", us->protocol_name);
	SPRINTF("    Transport: %s\n", us->transport_name);

	/* show the device flags */
	if (pos < buffer + length) {
		pos += sprintf(pos, "       Quirks:");
		f = us->flags;

#define DO_FLAG(a)  	if (f & US_FL_##a)  pos += sprintf(pos, " " #a)
		DO_FLAG(SINGLE_LUN);
		DO_FLAG(MODE_XLATE);
		DO_FLAG(START_STOP);
		DO_FLAG(IGNORE_SER);
		DO_FLAG(SCM_MULT_TARG);
		DO_FLAG(FIX_INQUIRY);
		DO_FLAG(FIX_CAPACITY);
#undef DO_FLAG

		*(pos++) = '\n';
		}

	/* release the reference count on this host */
        //PAD	scsi_host_put(hostptr);
	scsi_host_put(hostptr);

	/*
	 * Calculate start of next buffer, and return value.
	 */
	*start = buffer + offset;

	if ((pos - buffer) < offset)
		return (0);
	else if ((pos - buffer - offset) < length)
		return (pos - buffer - offset);
	else
		return (length);
}

/*
 * this defines our host template, with which we'll allocate hosts
 */

struct SHT usb_stor_host_template = {
	/* basic userland interface stuff */
	.name =				"usb-storage",
	.proc_name =			"usb-storage",
	.proc_info =			usb_storage_proc_info,
	.proc_dir =			NULL,
	.info =				usb_storage_info,
	.ioctl =			NULL,

	/* old-style detect and release */
	.detect =			NULL,
	.release =			NULL,

	/* command interface -- queued only */
	.command =			NULL,
	.queuecommand =			usb_storage_queuecommand,

	/* error and abort handlers */
	.eh_abort_handler =		usb_storage_command_abort,
	.eh_device_reset_handler =	usb_storage_device_reset,
	.eh_bus_reset_handler =		usb_storage_bus_reset,
	.eh_host_reset_handler =	NULL,
	.eh_strategy_handler =		NULL,

	/* queue commands only, only one command per LUN */
	.can_queue =			1,
	.cmd_per_lun =			1,

	/* unknown initiator id */
	.this_id =			-1,

	/* no limit on commands */
	.max_sectors =			0,
	
	/* pre- and post- device scan functions */
	.slave_alloc =			NULL,
	.slave_configure =		NULL,
	.slave_destroy =		NULL,

	/* lots of sg segments can be handled */
	.sg_tablesize =			SG_ALL,

	/* use 32-bit address space for DMA */
	.unchecked_isa_dma =		FALSE,
	.highmem_io =			FALSE,

	/* merge commands... this seems to help performance, but
	 * periodically someone should test to see which setting is more
	 * optimal.
	 */
	.use_clustering =		TRUE,

	/* emulated HBA */
	.emulated =			TRUE,

	/* sorry, no BIOS to help us */
	.bios_param =			NULL,

	/* module management */
	.module =			THIS_MODULE
};

/* For a device that is "Not Ready" */
unsigned char usb_stor_sense_notready[18] = {
	[0]	= 0x70,			    /* current error */
	[2]	= 0x02,			    /* not ready */
	[7]	= 0x0a,			    /* additional length */
	[12]	= 0x04,			    /* not ready */
	[13]	= 0x03			    /* manual intervention */
};


int usb_stor_scsiSense10to6( Scsi_Cmnd* the10 )
{
  __u8 *buffer=0;
  int outputBufferSize = 0;
  int length=0;
  struct scatterlist *sg = 0;
  int i=0, j=0, element=0;
  Usb_Stor_Scsi_Sense_Hdr_u the6Locations;
  Usb_Stor_Scsi_Sense_Hdr_10_u the10Locations;
  int sb=0,si=0,db=0,di=0;
  int sgLength=0;

  US_DEBUGP("-- converting 10 byte sense data to 6 byte\n");
  the10->cmnd[0] = the10->cmnd[0] & 0xBF;

  /* Determine buffer locations */
  usb_stor_scsiSenseParseBuffer( the10, &the6Locations, &the10Locations,
				 &length );

  /* Work out minimum buffer to output */
  outputBufferSize = *the10Locations.hdr.dataLengthLSB;
  outputBufferSize += USB_STOR_SCSI_SENSE_HDRSZ;

  /* Check to see if we need to trucate the output */
  if ( outputBufferSize > length )
    {
      printk( KERN_WARNING USB_STORAGE 
	      "Had to truncate MODE_SENSE_10 buffer into MODE_SENSE.\n" );
      printk( KERN_WARNING USB_STORAGE
	      "outputBufferSize is %d and length is %d.\n",
	      outputBufferSize, length );
    }
  outputBufferSize = length;

  /* Data length */
  if ( *the10Locations.hdr.dataLengthMSB != 0 ) /* MSB must be zero */
    {
      printk( KERN_WARNING USB_STORAGE 
	      "Command will be truncated to fit in SENSE6 buffer.\n" );
      *the6Locations.hdr.dataLength = 0xff;
    }
  else
    {
      *the6Locations.hdr.dataLength = *the10Locations.hdr.dataLengthLSB;
    }

  /* Medium type and DevSpecific parms */
  *the6Locations.hdr.mediumType = *the10Locations.hdr.mediumType;
  *the6Locations.hdr.devSpecParms = *the10Locations.hdr.devSpecParms;

  /* Block descriptor length */
  if ( *the10Locations.hdr.blkDescLengthMSB != 0 ) /* MSB must be zero */
    {
      printk( KERN_WARNING USB_STORAGE 
	      "Command will be truncated to fit in SENSE6 buffer.\n" );
      *the6Locations.hdr.blkDescLength = 0xff;
    }
  else
    {
      *the6Locations.hdr.blkDescLength = *the10Locations.hdr.blkDescLengthLSB;
    }

  if ( the10->use_sg == 0 )
    {
      buffer = the10->request_buffer;
      /* Copy the rest of the data */
      memmove( &(buffer[USB_STOR_SCSI_SENSE_HDRSZ]),
	       &(buffer[USB_STOR_SCSI_SENSE_10_HDRSZ]),
	       outputBufferSize - USB_STOR_SCSI_SENSE_HDRSZ );
      /* initialise last bytes left in buffer due to smaller header */
      memset( &(buffer[outputBufferSize
	    -(USB_STOR_SCSI_SENSE_10_HDRSZ-USB_STOR_SCSI_SENSE_HDRSZ)]),
	      0,
	      USB_STOR_SCSI_SENSE_10_HDRSZ-USB_STOR_SCSI_SENSE_HDRSZ );
    }
  else
    {
      sg = (struct scatterlist *) the10->request_buffer;
      /* scan through this scatterlist and figure out starting positions */
      for ( i=0; i < the10->use_sg; i++)
	{
	  sgLength = sg[i].length;
	  for ( j=0; j<sgLength; j++ )
	    {
	      /* get to end of header */
	      if ( element == USB_STOR_SCSI_SENSE_HDRSZ )
		{
		  db=i;
		  di=j;
		}
	      if ( element == USB_STOR_SCSI_SENSE_10_HDRSZ )
		{
		  sb=i;
		  si=j;
		  /* we've found both sets now, exit loops */
		  j=sgLength;
		  i=the10->use_sg;
		}
	      element++;
	    }
	}

      /* Now we know where to start the copy from */
      element = USB_STOR_SCSI_SENSE_HDRSZ;
      while ( element < outputBufferSize
	      -(USB_STOR_SCSI_SENSE_10_HDRSZ-USB_STOR_SCSI_SENSE_HDRSZ) )
	{
	  /* check limits */
	  if ( sb >= the10->use_sg ||
	       si >= sg[sb].length ||
	       db >= the10->use_sg ||
	       di >= sg[db].length )
	    {
	      printk( KERN_ERR USB_STORAGE
		      "Buffer overrun averted, this shouldn't happen!\n" );
	      break;
	    }

	  /* copy one byte */
	  {
		char *src = sg_address(sg[sb]) + si;
		char *dst = sg_address(sg[db]) + di;

		 *dst = *src;
	  }

	  /* get next destination */
	  if ( sg[db].length-1 == di )
	    {
	      db++;
	      di=0;
	    }
	  else
	    {
	      di++;
	    }

	  /* get next source */
	  if ( sg[sb].length-1 == si )
	    {
	      sb++;
	      si=0;
	    }
	  else
	    {
	      si++;
	    }

	  element++;
	}
      /* zero the remaining bytes */
      while ( element < outputBufferSize )
	{
	  /* check limits */
	  if ( db >= the10->use_sg ||
	       di >= sg[db].length )
	    {
	      printk( KERN_ERR USB_STORAGE
		      "Buffer overrun averted, this shouldn't happen!\n" );
	      break;
	    }

	  *(char*)(sg_address(sg[db])) = 0;

	  /* get next destination */
	  if ( sg[db].length-1 == di )
	    {
	      db++;
	      di=0;
	    }
	  else
	    {
	      di++;
	    }
	  element++;
	}
    }

  /* All done any everything was fine */
  return 0;
}

