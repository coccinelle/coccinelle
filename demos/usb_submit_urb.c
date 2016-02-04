// from usbnet.c 

static int gl_interrupt_read (struct usbnet *dev)
{
	struct gl_priv	*priv = dev->priv_data;
	int		retval;

	// issue usb interrupt read
	if (priv && priv->irq_urb) {
		// submit urb
		if ((retval = usb_submit_urb (priv->irq_urb)) != 0)
			dbg ("gl_interrupt_read: submit fail - %X...", retval);
		else
			dbg ("gl_interrupt_read: submit success...");
	}

	return 0;
}


static void rx_submit (struct usbnet *dev, struct urb *urb, int flags)
{
	struct sk_buff		*skb;
	struct skb_data		*entry;
	int			retval = 0;
	unsigned long		lockflags;
	size_t			size;

#ifdef CONFIG_USB_NET1080
	if (dev->driver_info->flags & FLAG_FRAMING_NC)
		size = FRAMED_SIZE (dev->net.mtu);
	else
#endif
#ifdef CONFIG_USB_GENESYS
	if (dev->driver_info->flags & FLAG_FRAMING_GL)
		size = GL_RCV_BUF_SIZE;
	else
#endif
		size = (sizeof (struct ethhdr) + dev->net.mtu);

	if ((skb = alloc_skb (size, flags)) == 0) {
		dbg ("no rx skb");
		tasklet_schedule (&dev->bh);
		usb_free_urb (urb);
		return;
	}

	entry = (struct skb_data *) skb->cb;
	entry->urb = urb;
	entry->dev = dev;
	entry->state = rx_start;
	entry->length = 0;

	FILL_BULK_URB (urb, dev->udev,
		usb_rcvbulkpipe (dev->udev, dev->driver_info->in),
		skb->data, size, rx_complete, skb);
	urb->transfer_flags |= USB_ASYNC_UNLINK;
#ifdef	REALLY_QUEUE
	urb->transfer_flags |= USB_QUEUE_BULK;
#endif
#if 0
	// Idle-but-posted reads with UHCI really chew up
	// PCI bandwidth unless FSBR is disabled
	urb->transfer_flags |= USB_NO_FSBR;
#endif

	spin_lock_irqsave (&dev->rxq.lock, lockflags);

	if (netif_running (&dev->net)) {
		if ((retval = usb_submit_urb (urb)) != 0) {
			dbg ("%s rx submit, %d", dev->net.name, retval);
			tasklet_schedule (&dev->bh);
		} else {
			__skb_queue_tail (&dev->rxq, skb);
		}
	} else {
		dbg ("rx: stopped");
		retval = -ENOLINK;
	}
	spin_unlock_irqrestore (&dev->rxq.lock, lockflags);
	if (retval) {
		dev_kfree_skb_any (skb);
		usb_free_urb (urb);
	}
}



static int usbnet_start_xmit (struct sk_buff *skb, struct net_device *net)
{
	struct usbnet		*dev = (struct usbnet *) net->priv;
	int			length = skb->len;
	int			retval = NET_XMIT_SUCCESS;
	struct urb		*urb = 0;
	struct skb_data		*entry;
	struct driver_info	*info = dev->driver_info;
	unsigned long		flags;
#ifdef	CONFIG_USB_NET1080
	struct nc_header	*header = 0;
	struct nc_trailer	*trailer = 0;
#endif	/* CONFIG_USB_NET1080 */

	flags = in_interrupt () ? GFP_ATOMIC : GFP_NOIO; /* might be used for nfs */

	// some devices want funky USB-level framing, for
	// win32 driver (usually) and/or hardware quirks
	if (info->tx_fixup) {
		skb = info->tx_fixup (dev, skb, flags);
		if (!skb) {
			dbg ("can't tx_fixup skb");
			goto drop;
		}
	}

	if (!(urb = usb_alloc_urb (0))) {
		dbg ("no urb");
		goto drop;
	}

	entry = (struct skb_data *) skb->cb;
	entry->urb = urb;
	entry->dev = dev;
	entry->state = tx_start;
	entry->length = length;

	// FIXME: reorganize a bit, so that fixup() fills out NetChip
	// framing too. (Packet ID update needs the spinlock...)

#ifdef	CONFIG_USB_NET1080
	if (info->flags & FLAG_FRAMING_NC) {
		header = (struct nc_header *) skb_push (skb, sizeof *header);
		header->hdr_len = cpu_to_le16 (sizeof (*header));
		header->packet_len = cpu_to_le16 (length);
		if (!((skb->len + sizeof *trailer) & 0x01))
			*skb_put (skb, 1) = PAD_BYTE;
		trailer = (struct nc_trailer *) skb_put (skb, sizeof *trailer);
	} else
#endif	/* CONFIG_USB_NET1080 */

	/* don't assume the hardware handles USB_ZERO_PACKET */
	if ((length % EP_SIZE (dev)) == 0)
		skb->len++;

	FILL_BULK_URB (urb, dev->udev,
			usb_sndbulkpipe (dev->udev, info->out),
			skb->data, skb->len, tx_complete, skb);
	urb->transfer_flags |= USB_ASYNC_UNLINK;
#ifdef	REALLY_QUEUE
	urb->transfer_flags |= USB_QUEUE_BULK;
#endif
	// FIXME urb->timeout = ... jiffies ... ;

	spin_lock_irqsave (&dev->txq.lock, flags);

#ifdef	CONFIG_USB_NET1080
	if (info->flags & FLAG_FRAMING_NC) {
		header->packet_id = cpu_to_le16 (dev->packet_id++);
		put_unaligned (header->packet_id, &trailer->packet_id);
#if 0
		devdbg (dev, "frame >tx h %d p %d id %d",
			header->hdr_len, header->packet_len,
			header->packet_id);
#endif
	}
#endif	/* CONFIG_USB_NET1080 */

	netif_stop_queue (net);
	if ((retval = usb_submit_urb (urb)) != 0) {
		netif_start_queue (net);
		dbg ("%s tx: submit urb err %d", net->name, retval);
	} else {
		net->trans_start = jiffies;
		__skb_queue_tail (&dev->txq, skb);
		if (dev->txq.qlen < TX_QLEN)
			netif_start_queue (net);
	}
	spin_unlock_irqrestore (&dev->txq.lock, flags);

	if (retval) {
		devdbg (dev, "drop, code %d", retval);
drop:
		retval = NET_XMIT_DROP;
		dev->stats.tx_dropped++;
		if (skb)
			dev_kfree_skb_any (skb);
		usb_free_urb (urb);
#ifdef	VERBOSE
	} else {
		devdbg (dev, "> tx, len %d, type 0x%x",
			length, skb->protocol);
#endif
	}
	return retval;
}
