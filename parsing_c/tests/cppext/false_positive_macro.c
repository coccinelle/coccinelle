// ID \n ID,  but first ID is not MacroSingle.

acpi_status
acpi_hw_register_read (
 	u8                              use_lock,
	u32                             register_id,
	u32                             *return_value)
{
}



// Not MacroSingle 
void main(int i) {
  conf = SAR_CFG_RXPTH|	/* enable receive path                  */
    SAR_RX_DELAY |	/* interrupt on complete PDU		*/
    SAR_CFG_RAWIE |	/* interrupt enable on raw cells        */
    SAR_CFG_RQFIE |	/* interrupt on RSQ almost full         */
    SAR_CFG_TMOIE |	/* interrupt on timer overflow          */
    SAR_CFG_FBIE |	/* interrupt on low free buffers        */
    SAR_CFG_TXEN |	/* transmit operation enable            */
    SAR_CFG_TXINT |	/* interrupt on transmit status         */
    SAR_CFG_TXUIE |	/* interrupt on transmit underrun       */
    SAR_CFG_TXSFI |	/* interrupt on TSQ almost full         */
    SAR_CFG_PHYIE	/* enable PHY interrupts		*/
    ;


}

// ?
static void pcnet32_set_multicast_list(struct net_device *dev)
{
	unsigned long ioaddr = dev->base_addr, flags;
	struct pcnet32_private *lp = dev->priv;

	spin_lock_irqsave(&lp->lock, flags);
	if (dev->flags & IFF_PROMISC) {
		/* Log any net taps. */
		if (netif_msg_hw(lp))
			printk(KERN_INFO "%s: Promiscuous mode enabled.\n",
			       dev->name);
		lp->init_block.mode =
		    le16_to_cpu(0x8000 | (lp->options & PCNET32_PORT_PORTSEL) <<
				7);
	} else {
		lp->init_block.mode =
		    le16_to_cpu((lp->options & PCNET32_PORT_PORTSEL) << 7);
		pcnet32_load_multicast(dev);
	}

	lp->a.write_csr(ioaddr, 0, 0x0004);	/* Temporarily stop the lance. */
	pcnet32_restart(dev, 0x0042);	/*  Resume normal operation */
	netif_wake_queue(dev);

	spin_unlock_irqrestore(&lp->lock, flags);
}


// Not MacroNoPtVirg 
static struct snd_kcontrol_new snd_ali5451_mixer_spdif[] __devinitdata = {
	/* spdif aplayback switch */
	/* FIXME: "IEC958 Playback Switch" may conflict with one on ac97_codec */
	ALI5451_SPDIF(SNDRV_CTL_NAME_IEC958("Output ",NONE,SWITCH), 0, 0),
	/* spdif out to spdif channel */
	ALI5451_SPDIF(SNDRV_CTL_NAME_IEC958("Channel Output ",NONE,SWITCH), 0, 1),
	/* spdif in from spdif channel */
	ALI5451_SPDIF(SNDRV_CTL_NAME_IEC958("",CAPTURE,SWITCH), 0, 2)
};


// Not MacroNoPtVirg 
static struct serio_dev ps2serkbd_dev = {
interrupt:
    ps2serkbd_interrupt,
connect:
    ps2serkbd_connect,
disconnect:
    ps2serkbd_disconnect
};


// Not MacroNoPtVirg
int i = {
		{ DMA_RQ_C1_SOURCE_ON_HOST +        /* source buffer is on the host */
		  DMA_RQ_C1_SOURCE_MOD1024 +        /* source buffer is 1024 dwords (4096 bytes) */
		  DMA_RQ_C1_DEST_MOD32 +            /* dest buffer(PCMreaderBuf) is 32 dwords*/
		  DMA_RQ_C1_WRITEBACK_SRC_FLAG +    /* ?? */
		  DMA_RQ_C1_WRITEBACK_DEST_FLAG +   /* ?? */
		  15,                             /* DwordCount-1: picked 16 for DwordCount because Jim */
		  /*        Barnette said that is what we should use since */
		  /*        we are not running in optimized mode? */
		  DMA_RQ_C2_AC_NONE +
		  DMA_RQ_C2_SIGNAL_SOURCE_PINGPONG + /* set play interrupt (bit0) in HISR when source */
		  /*   buffer (on host) crosses half-way point */
		  virtual_channel,                   /* Play DMA channel arbitrarily set to 0 */
		  playback_hw_addr,                  /* HostBuffAddr (source) */
		  DMA_RQ_SD_SP_SAMPLE_ADDR +         /* destination buffer is in SP Sample Memory */
		  sample_buffer_addr                 /* SP Buffer Address (destination) */
 		},
};


// Not MacroSingle
enum {
	GEMTEK_PR103
};



// Pb with DBG ?
void main(int i)
{
  if (1) { }
  else
    snd_emu10k1_ptr_write(emu, DBG, 0, emu->fx8010.dbg |= EMU10K1_DBG_SINGLE_STEP);
  return 0;
}


int x = {
// Not MacroNoPtVirg 
	.private_value = IEC958_AES0_NONAUDIO |
			IEC958_AES0_PROFESSIONAL |
			IEC958_AES0_CON_EMPHASIS
};


