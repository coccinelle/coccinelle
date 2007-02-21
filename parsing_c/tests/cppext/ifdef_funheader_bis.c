#if LINUX_VERSION_CODE < KERNEL_VERSION(2, 3, 0)
static void* se401_probe(struct usb_device *dev, unsigned int ifnum)
#else
static void* __devinit se401_probe(struct usb_device *dev, unsigned int ifnum,
	const struct usb_device_id *id)
#endif
{
}


#if defined(LINUX_2_1) || defined(LINUX_2_4)
static int if_rebuild_hdr (struct sk_buff* skb)
{
#else
static int if_rebuild_hdr (void* hdr, netdevice_t* dev, unsigned long raddr,
                           struct sk_buff* skb)
{
}


#if defined(OS2)
void far phaseStatus(ULONG port, UCHAR p_card)
#else
#if defined(DOS)
void phaseStatus(USHORT port, UCHAR p_card)
#else
void phaseStatus(ULONG port, UCHAR p_card)
#endif
#endif
{
}

#if defined(OS2)
void far phaseMsgOut(ULONG port, UCHAR p_card)
#else
#if defined(DOS)
void phaseMsgOut(USHORT port, UCHAR p_card)
#else
void phaseMsgOut(ULONG port, UCHAR p_card)
#endif
#endif
{
}

