static int usbin_start(struct usb_audiodev *as)
{
  struct usb_device *dev = as->state->usbdev;
  struct usbin *u = &as->usbin;
  struct urb *urb;
  unsigned long flags;
  unsigned int maxsze, bufsz;
  
  /* allocate USB storage if not already done */
  spin_lock_irqsave(&as->lock, flags);
  if (!(u->flags & FLG_CONNECTED)) {
    spin_unlock_irqrestore(&as->lock, flags);
    goto end;
  }
  if (!(u->flags & FLG_RUNNING)) {
    spin_unlock_irqrestore(&as->lock, flags);
    u->freqn = ((u->dma.srate << 11) + 62) / 125; /* this will overflow at approx 2MSPS */
    u->freqmax = u->freqn + (u->freqn >> 2);
    u->phase = 0;
    maxsze = (u->freqmax + 0x3fff) >> (14 - AFMT_BYTESSHIFT(u->format));
    bufsz = DESCFRAMES * maxsze;
    if (u->durb[0].urb->transfer_buffer)
      kfree(u->durb[0].urb->transfer_buffer);
    u->durb[0].urb->transfer_buffer = kmalloc(bufsz, GFP_KERNEL);
    u->durb[0].urb->transfer_buffer_length = bufsz;
    if (u->durb[1].urb->transfer_buffer)
      kfree(u->durb[1].urb->transfer_buffer);
    u->durb[1].urb->transfer_buffer = kmalloc(bufsz, GFP_KERNEL);
    u->durb[1].urb->transfer_buffer_length = bufsz;
    if (u->syncpipe) {
      if (u->surb[0].urb->transfer_buffer)
	kfree(u->surb[0].urb->transfer_buffer);
      u->surb[0].urb->transfer_buffer = kmalloc(3*SYNCFRAMES, GFP_KERNEL);
      u->surb[0].urb->transfer_buffer_length = 3*SYNCFRAMES;
      if (u->surb[1].urb->transfer_buffer)
	kfree(u->surb[1].urb->transfer_buffer);
      u->surb[1].urb->transfer_buffer = kmalloc(3*SYNCFRAMES, GFP_KERNEL);
      u->surb[1].urb->transfer_buffer_length = 3*SYNCFRAMES;
    }
    if (!u->durb[0].urb->transfer_buffer || !u->durb[1].urb->transfer_buffer || 
	(u->syncpipe && (!u->surb[0].urb->transfer_buffer || !u->surb[1].urb->transfer_buffer))) {
      printk(KERN_ERR "usbaudio: cannot start playback device %d\n", dev->devnum);
      goto end;
    }
    spin_lock_irqsave(&as->lock, flags);
  }
  if (u->dma.count >= u->dma.dmasize && !u->dma.mapped) {
    spin_unlock_irqrestore(&as->lock, flags);
    goto end;
  }
  u->flags |= FLG_RUNNING;
  if (!(u->flags & FLG_URB0RUNNING)) {
    urb = u->durb[0].urb;
    urb->dev = dev;
    urb->pipe = u->datapipe;
    urb->transfer_flags = URB_ISO_ASAP;
    urb->number_of_packets = DESCFRAMES;
    urb->context = as;
    urb->complete = usbin_completed;
    if (!usbin_prepare_desc(u, urb) && !usb_submit_urb(urb, GFP_KERNEL))
      u->flags |= FLG_URB0RUNNING;
    else
      u->flags &= ~FLG_RUNNING;
  }
  if (u->flags & FLG_RUNNING && !(u->flags & FLG_URB1RUNNING)) {
    urb = u->durb[1].urb;
    urb->dev = dev;
    urb->pipe = u->datapipe;
    urb->transfer_flags = URB_ISO_ASAP;
    urb->number_of_packets = DESCFRAMES;
    urb->context = as;
    urb->complete = usbin_completed;
    if (!usbin_prepare_desc(u, urb) && !usb_submit_urb(urb, GFP_KERNEL))
      u->flags |= FLG_URB1RUNNING;
    else
      u->flags &= ~FLG_RUNNING;
  }
  if (u->syncpipe) {
    if (u->flags & FLG_RUNNING && !(u->flags & FLG_SYNC0RUNNING)) {
      urb = u->surb[0].urb;
      urb->dev = dev;
      urb->pipe = u->syncpipe;
      urb->transfer_flags = URB_ISO_ASAP;
      urb->number_of_packets = SYNCFRAMES;
      urb->context = as;
      urb->complete = usbin_sync_completed;
      /* stride: u->syncinterval */
      if (!usbin_sync_prepare_desc(u, urb) && !usb_submit_urb(urb, GFP_KERNEL))
	u->flags |= FLG_SYNC0RUNNING;
      else
	u->flags &= ~FLG_RUNNING;
    }
    if (u->flags & FLG_RUNNING && !(u->flags & FLG_SYNC1RUNNING)) {
      urb = u->surb[1].urb;
      urb->dev = dev;
      urb->pipe = u->syncpipe;
      urb->transfer_flags = URB_ISO_ASAP;
      urb->number_of_packets = SYNCFRAMES;
      urb->context = as;
      urb->complete = usbin_sync_completed;
      /* stride: u->syncinterval */
      if (!usbin_sync_prepare_desc(u, urb) && !usb_submit_urb(urb, GFP_KERNEL))
	u->flags |= FLG_SYNC1RUNNING;
      else
	u->flags &= ~FLG_RUNNING;
    }
  }
  spin_unlock_irqrestore(&as->lock, flags);
end:
  return 0;
}
