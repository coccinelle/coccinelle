void main(int i) 
{

  tmp = (hostdata->script + hostdata->script_count);
  hostdata->free = ROUNDUP(tmp, void *);

  ACPI_DEBUGGER_EXEC (status = acpi_db_single_step (walk_state, op, op_class));
  ACPI_DEBUGGER_EXEC (if (ACPI_FAILURE (status)) {return_ACPI_STATUS (status);});

  snd_assert(pcm != NULL, return -EINVAL);


  ASSERT(cb != NULL, return;);

  DEB(if (target >= NR_FAKE_DISKS) {
    SCpnt->result = DID_TIME_OUT << 16; done(SCpnt); return 0;
    }
  );

  DEB(if (target >= NR_FAKE_DISKS) {
       SCpnt->result = DID_TIME_OUT << 16; done(SCpnt); return 0;
    }
  );


  DBGINFO(
	printk("vendor_id = %x\n", vendor_id);
	printk("device_id = %x\n", device_id);
	printk("command = %x\n", command);
	for(i=0; i<6; i++)
		printk("addr[%d] = %lx\n", i, addr[i]);
	printk("revision = %x\n", revision);
	printk("irq = %x\n", irq);
	printk("cache_line_size = %x\n", cache_line_size);
	printk("latency_timer = %x\n", latency_timer);
	printk("board_id = %x\n", board_id);
  );


  ACPI_DEBUGGER_EXEC (status = acpi_db_single_step (walk_state, op, op_class));
  ACPI_DEBUGGER_EXEC (if (ACPI_FAILURE (status)) {return_ACPI_STATUS (status);});


  cb = (struct irda_skb_cb *) skb->cb;
  ASSERT(cb != NULL, return;);
  self = (struct irda_usb_cb *) cb->context;
  ASSERT(self != NULL, return;);


  IRDA_ASSERT ((physaddr & 0x3ff) == 0,
 	       printk (KERN_ERR DRIVER_NAME "ring not correctly aligned\n");
 	       return;);

  IRDA_ASSERT (self != NULL, return 0; );


  EISA_DBG("EISA edge/level %04x\n", eisa_irq_level);

  eisa_out8(eisa_irq_level&0xff, 0x4d0); 

  eisa_eeprom_addr = ioremap_nocache(eisa_dev.eeprom_addr, HPEE_MAX_LENGTH);
  result = eisa_enumerator(eisa_dev.eeprom_addr, &eisa_dev.hba.io_space,
 			&eisa_dev.hba.lmmio_space);
  init_eisa_pic();

  *R_DMA_CH0_CLR_INTR = IO_STATE(R_DMA_CH0_CLR_INTR, clr_eop, do);
  *R_DMA_CH1_CLR_INTR = IO_STATE(R_DMA_CH1_CLR_INTR, clr_eop, do);

  *R_DMA_CH1_CLR_INTR = IO_STATE(R_DMA_CH1_CLR_INTR, clr_eop, do);

  DEB (if(*cdb) printk ("\nCDB: %X-  %X %X %X %X %X %X %X %X %X %X ", SCpnt->cmd_len, cdb[0], cdb[1], cdb[2], cdb[3], cdb[4], cdb[5], cdb[6], cdb[7], cdb[8], cdb[9]));
  DEB (if(*cdb) printk ("\ntimeout_per_command: %d, timeout_total: %d, timeout: %d, internal_timout: %d", SCpnt->timeout_per_command,
 							  SCpnt->timeout_total, SCpnt->timeout, SCpnt->internal_timeout));
  outl (SCpnt->timeout_per_command, padapter->mb1);
}


void main(int i) 
{


  *info->ocmdadr = IO_STATE(R_DMA_CH6_CMD, cmd, continue);
  restore_flags(flags);
  return;

  ASSERT(dev != NULL, return -1;);

  snd_assert(chip >= 0 && chip < 4, return);

  snd_runtime_check(copy_from_user(&id, _id, sizeof(id)) == 0, continue);

  snd_assert(gp, return);

  snd_assert(gp, return);


  snd_assert (cpcm->pcm_channel != NULL);
  
  snd_assert ( pcm == chip->pcm_dig ); 

  *R_DMA_CH8_SUB0_CLR_INTR = IO_STATE(R_DMA_CH8_SUB0_CLR_INTR, clr_descr, do);


  IO_STATE(R_DMA_CH6_CLR_INTR, clr_descr, do) | 1;
  IO_STATE(R_DMA_CH6_CLR_INTR, clr_eop, do);


  D3({
		char *s = (char *)kmalloc(len + 1, GFP_KERNEL);
		memcpy(s, name, len);
		s[len] = '\0';
		printk("jffs_lookup(): dir: 0x%p, name: \"%s\"\n", dir, s);
		kfree(s);
  });

  D3(printk (KERN_NOTICE "lookup(): down biglock\n"));


  DEBUG1(struct zoran *zr = (struct zoran *) bus->data);


  ASSERT(priv != NULL, return;);


  /* Make sure the irqs are cleared */
  *info->iclrintradr =
    IO_STATE(R_DMA_CH6_CLR_INTR, clr_descr, do) |
    IO_STATE(R_DMA_CH6_CLR_INTR, clr_eop, do);

//parse error 
// = File "test_c/bugs/80/errors/linux-2.6.10/drivers/serial/crisv10.c", line 2254, characters 42
//    around = 'do', whole content = 		IO_STATE(R_DMA_CH6_CLR_INTR, clr_descr, do) |
// charpos = 70035
//FOUND SYNC at line 2299

  snd_runtime_check(snd_i2c_sendbytes(ice->cs8404, &bits, 1) == 1, goto _error);

  snd_runtime_check(gctl.id.iface == SNDRV_CTL_ELEM_IFACE_MIXER ||
		                  gctl.id.iface == SNDRV_CTL_ELEM_IFACE_PCM, continue);


//parse error 
// = File "test_c/bugs/83/errors/linux-2.6.10/drivers/serial/crisv10.c", line 3040, characters 48
//    around = 'continue', whole content = 		*info->ocmdadr = IO_STATE(R_DMA_CH6_CMD, cmd, continue);
// charpos = 93859
}
