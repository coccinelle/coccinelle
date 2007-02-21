#ifdef MODULE
int init_module(void)
#else
static int __init cosa_init(void)
#endif
{
}


#ifndef PCI_OLD_SUSPEND
static int snd_ali_suspend(struct pci_dev *dev, u32 state)
#else
static void snd_ali_suspend(struct pci_dev *dev)
#endif
{
}


#ifdef MODULE
int __init __sbpcd_init(void)
#else
int __init sbpcd_init(void)
#endif
{
}

#ifdef MODULE
int __init __sbpcd_init(void)
#else
int __init sbpcd_init(void)
#endif
{
	char nbuff[16];
}


#if LINUX_VERSION_CODE >= 0x020503
int gdth_bios_param(struct scsi_device *sdev,struct block_device *bdev,sector_t cap,int *ip)
#elif LINUX_VERSION_CODE >= 0x010300
int gdth_bios_param(Disk *disk,kdev_t dev,int *ip)
#else
int gdth_bios_param(Disk *disk,int dev,int *ip)
#endif
{
}


#ifdef HH_VERSION
static void audio_dma_callback(void *data, int size)
#else
static void audio_dma_callback(void *data)
#endif
{
	audio_stream_t *s = data;
        
	/* 
	 * If we are getting a callback for an active stream then we inform
	 * the PCM middle layer we've finished a period
	 */
 	if (s->active)
		snd_pcm_period_elapsed(s->stream);

	spin_lock(&s->dma_lock);
	if (!s->tx_spin && s->periods > 0)
		s->periods--;
	audio_process_dma(s);
	spin_unlock(&s->dma_lock);
}


#if LINUX_VERSION_CODE >= 0x010346
int gdth_reset(Scsi_Cmnd *scp, unsigned int reset_flags)
#else
int gdth_reset(Scsi_Cmnd *scp)
#endif
{
    TRACE2(("gdth_reset()\n"));
    return SCSI_RESET_PUNT;
}


#if defined(DOS)
USHORT SccbMgr_config_adapter(PSCCBMGR_INFO pCardInfo)
#else
ULONG SccbMgr_config_adapter(PSCCBMGR_INFO pCardInfo)
#endif
{
}



#if (FW_TYPE==_UCB_MGR_)
void SccbMgr_unload_card(CARD_HANDLE pCurrCard)
#else
#if defined(DOS)
void SccbMgr_unload_card(USHORT pCurrCard)
#else
void SccbMgr_unload_card(ULONG pCurrCard)
#endif
#endif
{
}
