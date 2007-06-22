static int __init agp_i7x05_probe (struct pci_dev *dev, const struct pci_device_id *ent)
{
	u8 cap_ptr = 0;

	cap_ptr = pci_find_capability(dev, PCI_CAP_ID_AGP);
	if (cap_ptr == 0)
		return -ENODEV;

	if (agp_lookup_host_bridge(dev) != -ENODEV) {
		agp_bridge.dev = dev;
		agp_bridge.capndx = cap_ptr;
		/* Fill in the mode register */
		pci_read_config_dword(agp_bridge.dev, agp_bridge.capndx+PCI_AGP_STATUS, &agp_bridge.mode)
		agp_register_driver(dev);
		return 0;
	}
	return -ENODEV;
}


void main(int i) 
{
  struct Scsi_Host *dev = dev_id;
  DEB(unsigned char fifo_size;)
  DEB(unsigned char seq_reg;)

  DEBUG2(BUG();)

  DEB(printk("cmd=%02x, cmd_len=%02x, target=%02x, lun=%02x, bufflen=%d\n", 
               SCpnt->cmnd[0], SCpnt->cmd_len, SCpnt->device->id, 
               SCpnt->device->lun,  SCpnt->request_bufflen));
 
  VDEB(for (i = 0; i < SCpnt->cmd_len; i++)
       printk("cmd[%d]=%02x  ", i, SCpnt->cmnd[i]));
  VDEB(printk("\n"));

  DEB(printk("SYM53C500: port_base=0x%x, irq=%d, fast_pio=%d\n",
             port_base, irq_level, USE_FAST_PIO);)
    
  chip_init(port_base);



  IF_ABR(printk(" Desc %d is reset at %ld\n", desc1 -1, jiffies);)
  *(u_short *) (dev->seg_ram + dev->host_tcq_wr) = 0;

  IF_ABR(printk("LOCK UP found\n");) 
  writew(0xFFFD, dev->seg_reg+MODE_REG_0);

  if (iadev->NumEnabledCBR == 0) {
    writew((UBR_EN | ABR_EN | (0x23 << 2)), iadev->seg_reg+STPARMS);
    IF_CBR (printk("CBR support disabled\n");)
  }

  IF_INIT(printk("Tx MASK REG: 0x%0x\n", 
                                 readw(iadev->seg_reg+SEG_MASK_REG));)  

  DBG_LOOP(__FUNCTION__)

  DBG(__FUNCTION__)
 
  switch (bpp) {
  case 4:	 if (!ACCESS_FBINFO(capable.cfb4)) return -EINVAL;
    break;
  case 8:	 break;
  case 16: break;
  case 24: break;
  case 32: break;
  default: return -EINVAL;
  }



  SOD (("soc_solicited, %d pkts arrived\n", (sw_cq->in-sw_cq->out) & sw_cq->last))
  for (;;) { }


  test3(printk("MCD_S_DATA\n"));
  st = inb(MCDPORT(1)) & (MFL_STATUSorDATA);
  data_immediately:
  test5(printk("Status %02x\n", st))
  f();



  __SIMPLE_OPTION(PCI_PARITY, pci_parity)
  __SIMPLE_OPTION(SCSI_PARITY, scsi_parity)
  __SIMPLE_OPTION(MIN_SYNC, min_sync)
  __SIMPLE_OPTION(BURST_ORDER, burst_order)
  __SIMPLE_OPTION(SCSI_LED, scsi_led)
  __SIMPLE_OPTION(MAX_WIDE, max_wide)
  __SIMPLE_OPTION(SCSI_DIFF, scsi_diff)
  __SIMPLE_OPTION(IRQ_MODE, irq_mode)

  UCHAR   bval;
  DC390_AFLAGS
  PACB    pACB = (PACB) cmd->device->host->hostdata;

  DBG(3, "I2C read failed for %s image sensor", sensor->name)
 
  PDBGG("I2C read: address 0x%02X, value: 0x%02X", address, data[4])


  DBG(1, "Not enough memory")
  goto free_buffers;

}

void main(int i)
{

  
  DEBUG(printk("qla2x00_find_propname: found "
               "property = {%s}\n",
               propstr);)

  PDBGG("Window size adjusted w=%d, h=%d ", *width, *height)
  return 0;


#define __SIMPLE_OPTION(NAME, name) \
		case OPT_ ## NAME :		\
			sym_driver_setup.name = val;\
			break;

  __SIMPLE_OPTION(PCI_PARITY, pci_parity)
  __SIMPLE_OPTION(SCSI_PARITY, scsi_parity)
  __SIMPLE_OPTION(MIN_SYNC, min_sync)

  DEBUG0(printk ("DC390: Append cmd %li to Query\n", cmd->pid);)




    if (!schedule_timeout(tmo + 1)) {
      DBG(printk(KERN_DEBUG PFX "dac1 dma timed out??\n");)
    }

    DEBUG(printk
	      (KERN_DEBUG "SAA7185: %02x set to %02x\n", subaddr, data);
    )
    LOCK_I2C_BUS(dev->bus);

    DB(DB_QUEUE_COMMAND,
	   printk("Q-%d-%02x-%ld( ", cmd->device->id, cmd->cmnd[0], cmd->pid))


    DB(DB_TRANSFER,
	   printk("(%p,%d,%s:", buf, cnt, data_in_dir ? "in" : "out"))

    write_wd33c93(regs, WD_CONTROL, CTRL_IDI | CTRL_EDI | CTRL_POLLED);


}

void main(int i)
{

    DB(DB_EXECUTE, printk("EX("))
    if (hostdata->selecting || hostdata->connected) {
		DB(DB_EXECUTE, printk(")EX-0 "))
		return;

		CHECK_NULL(cmd, "fifo_int")

                if (hostdata->fifo == FI_FIFO_READING) {
                }
    }

   DB(DB_EXECUTE, printk("EX("))
     
   if (hostdata->selecting || hostdata->connected) {}

   PCI_TRACEO( (ULONG)Cmnd, 0x98)
      
  
   Cmnd->scsi_done = done;

   FCALD(("trying DID %06x\n", fcmd->did))
   return 0;

   DBG("PLL_calcclock")

   scrlen = htotal * (vtotal - 1);


   if (!schedule_timeout(tmo + 1))
			DBG(printk(KERN_DEBUG PFX "dac2 dma timed out??\n");)


}

void main(int i)
{

   DEB(int i);


   test5(printk("Status %02x\n", st))
   switch (st) {
   case MFL_DATA:

     ECALL(fd_copyin((void *)param, &inparam, size))

   }

   PCI_TRACEO( (ULONG)Cmnd, 0x98)
      
  
   Cmnd->scsi_done = done;


   DBG(printk(KERN_DEBUG "es1370: dma timed out??\n");)


   ECALL(fd_copyin((void *)param, &inparam, size))


   if (res < 0)
		DBG(4, "Failed to read a register "
		       "(index 0x%02X, error #%d, %s)",
		    index, res, symbolic(urb_errlist, res))

   return (res >= 0) ? (int)(*buff) : -1;

   if ((err = usb_register(&sn9c102_usb_driver)))
		KDBG(1, "usb_register() failed")

}

