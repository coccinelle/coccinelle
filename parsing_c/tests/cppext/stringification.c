void main() 
{
  if (option & 15)
    printk(MY_KERN_INFO ANDOTHER "%s: ignoring user supplied media type %d",
           dev->name, option & 15);

  out += sprintf(out, "Load type: " MODULEPROCSTRING "\n\n");

  printk(KERN_INFO BANNER);

  printk (NAME53C8XX ": failed to allocate %s[%d]\n", name, size);

  printk(KERN_INFO NAME53C "%s-%d: rev 0x%x on pci bus %d device %d function %d ");
  printk(KERN_INFO NAME53C "%s-%d: rev 0x%x on pci bus %d device %d function %d ");

  if ((rc = mpt_do_ioc_recovery(ioc, MPT_HOSTEVENT_IOC_RECOVER, sleepFlag)) != 0) {
    printk(KERN_WARNING MYNAM ": WARNING - (%d) Cannot recover %s\n",
           rc, ioc->name);
  }
  printk (NAME53C8XX ": setup=disc:%c,specf:%d,tags:%d,sync:%d,"
          "burst:%d,wide:%c,diff:%d,revprob:%c,buschk:0x%x\n",
          YesNo(driver_setup.disconnection),
          driver_setup.special_features,
          driver_setup.default_tags,
          driver_setup.default_sync,
          driver_setup.burst_max,
          YesNo(driver_setup.max_wide),
          driver_setup.diff_support,
          YesNo(driver_setup.reverse_probe),
          driver_setup.bus_check);

  printk(KERN_INFO NAME53C8XX ": 53c%s detected %s\n",
         devp->chip.name, msg);


  copy_info(&info, "General information:\n");
  copy_info(&info, "  Chip " NAME53C "%s, device id 0x%x, "
            "revision id 0x%x\n",
            np->chip_name, np->device_id,	np->revision_id);

  printk(KERN_INFO "Uniform CD-ROM driver " REVISION "\n");
  
  US_DEBUGP("Found existing GUID " GUID_FORMAT "\n",
            GUID_ARGS(guid));


}

void main() 
{

		nehprintk((KERN_WARNING MYNAM ": mptscsih_bus_reset: "
			   "Can't locate host! (sc=%p)\n",
			   SCpnt ) );
		return FAILED;
}


void main() 
{

	printk(KERN_INFO PFX "found es1371 rev %d at io %#lx irq %u\n"
	       KERN_INFO PFX "features: joystick 0x%x\n", s->rev, s->io, s->irq, joystick[devindex]);


	t = scnprintf(next, size, DRIVER_DESC "\n"
		"%s version: %s\nGadget driver: %s\nHost %s\n\n",
		driver_name, DRIVER_VERSION SIZE_STR DMASTR,
		dev->driver ? dev->driver->driver.name : "(none)",
		is_vbus_present() ? "full speed" : "disconnected");
	size -= t;
	next += t;

	pr_debug("%s: IRQ %d%s%s%s\n", driver_name, IRQ_USB,
		dev->has_cfr ? "" : " (!cfr)",
		out_dma ? "" : " (broken dma-out)",
		SIZE_STR DMASTR
		);


	if (snd_register_ioport(card,
			pnp ? fm_port : fm_port = 0x388, 4,
			DRIVER_NAME" - OPL", NULL) < 0)
          {


            snd_printk(KERN_ERR PFX "opl3 not detected at 0x%lx\n", chip->fm_port);
          } else {
          }
}
