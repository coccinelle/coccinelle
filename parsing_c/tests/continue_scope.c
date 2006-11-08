int __init
setup_netjet_s(struct IsdnCard *card)
{
	int bytecnt;
	struct IsdnCardState *cs = card->cs;
	char tmp[64];
	unsigned long flags;

#ifdef __BIG_ENDIAN
#error "not running on big endian machines now"
#endif
	strcpy(tmp, NETjet_S_revision);
	printk(KERN_INFO "HiSax: Traverse Tech. NETjet-S driver Rev. %s\n", HiSax_getrev(tmp));
	if (cs->typ != ISDN_CTYPE_NETJET_S)
		return(0);
	test_and_clear_bit(FLG_LOCK_ATOMIC, &cs->HW_Flags);

#if CONFIG_PCI

	for ( ;; )
	{
		if (!pci_present()) {
			printk(KERN_ERR "Netjet: no PCI bus present\n");
			return(0);
		}
		if ((dev_netjet = pci_find_device(PCI_VENDOR_ID_TIGERJET,
			PCI_DEVICE_ID_TIGERJET_300,  dev_netjet))) {
			if (pci_enable_device(dev_netjet))
				return(0);
			pci_set_master(dev_netjet);
			cs->irq = dev_netjet->irq;
			if (!cs->irq) {
				printk(KERN_WARNING "NETjet-S: No IRQ for PCI card found\n");
				return(0);
			}
			cs->hw.njet.base = pci_resource_start(dev_netjet, 0);
			if (!cs->hw.njet.base) {
				printk(KERN_WARNING "NETjet-S: No IO-Adr for PCI card found\n");
				return(0);
			}
 			/* 2001/10/04 Christoph Ersfeld, Formula-n Europe AG www.formula-n.com */
 			if ((dev_netjet->subsystem_vendor == 0x55) &&
 				(dev_netjet->subsystem_device == 0x02)) {
 				printk(KERN_WARNING "Netjet: You tried to load this driver with an incompatible TigerJet-card\n");
 				printk(KERN_WARNING "Use type=41 for Formula-n enter:now ISDN PCI and compatible\n");
 				return(0);
 			}
 			/* end new code */
			cs->hw.njet.pdev = dev_netjet;
		} else {
			printk(KERN_WARNING "NETjet-S: No PCI card found\n");
			return(0);
		}

		cs->hw.njet.auxa = cs->hw.njet.base + NETJET_AUXDATA;
		cs->hw.njet.isac = cs->hw.njet.base | NETJET_ISAC_OFF;

		save_flags(flags);
		sti();

		cs->hw.njet.ctrl_reg = 0xff;  /* Reset On */
		byteout(cs->hw.njet.base + NETJET_CTRL, cs->hw.njet.ctrl_reg);

		set_current_state(TASK_UNINTERRUPTIBLE);
		schedule_timeout((10*HZ)/1000);	/* Timeout 10ms */

		cs->hw.njet.ctrl_reg = 0x00;  /* Reset Off and status read clear */
		byteout(cs->hw.njet.base + NETJET_CTRL, cs->hw.njet.ctrl_reg);

		set_current_state(TASK_UNINTERRUPTIBLE);
		schedule_timeout((10*HZ)/1000);	/* Timeout 10ms */

		restore_flags(flags);

		cs->hw.njet.auxd = 0xC0;
		cs->hw.njet.dmactrl = 0;

		byteout(cs->hw.njet.base + NETJET_AUXCTRL, ~NETJET_ISACIRQ);
		byteout(cs->hw.njet.base + NETJET_IRQMASK1, NETJET_ISACIRQ);
		byteout(cs->hw.njet.auxa, cs->hw.njet.auxd);

		switch ( ( ( NETjet_ReadIC( cs, ISAC_RBCH ) >> 5 ) & 3 ) )
		{
			case 0 :
				break;

			case 3 :
				printk( KERN_WARNING "NETjet-S: NETspider-U PCI card found\n" );
				continue;

			default :
				printk( KERN_WARNING "NETjet-S: No PCI card found\n" );
				return 0;
                }
                break;
	}
#else

	printk(KERN_WARNING "NETjet-S: NO_PCI_BIOS\n");
	printk(KERN_WARNING "NETjet-S: unable to config NETJET-S PCI\n");
        //PAD        return (0);

#endif /* CONFIG_PCI */

	bytecnt = 256;

	printk(KERN_INFO
		"NETjet-S: PCI card configured at %#lx IRQ %d\n",
		cs->hw.njet.base, cs->irq);
	if (check_region(cs->hw.njet.base, bytecnt)) {
		printk(KERN_WARNING
		       "HiSax: %s config port %#lx-%#lx already in use\n",
		       CardType[card->typ],
		       cs->hw.njet.base,
		       cs->hw.njet.base + bytecnt);
		return (0);
	} else {
		request_region(cs->hw.njet.base, bytecnt, "netjet-s isdn");
	}
	reset_netjet_s(cs);
	cs->readisac  = &NETjet_ReadIC;
	cs->writeisac = &NETjet_WriteIC;
	cs->readisacfifo  = &NETjet_ReadICfifo;
	cs->writeisacfifo = &NETjet_WriteICfifo;
	cs->BC_Read_Reg  = &dummyrr;
	cs->BC_Write_Reg = &dummywr;
	cs->BC_Send_Data = &netjet_fill_dma;
	cs->cardmsg = &NETjet_S_card_msg;
	cs->irq_func = &netjet_s_interrupt;
	cs->irq_flags |= SA_SHIRQ;
	ISACVersion(cs, "NETjet-S:");
	return (1);
}
