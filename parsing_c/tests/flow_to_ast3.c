static void hci_usb_bulk_read(struct urb *urb)
{
	struct hci_usb *husb = (struct hci_usb *) urb->context;
	unsigned char *data = urb->transfer_buffer;
	int count = urb->actual_length, status;
	struct sk_buff *skb;
	hci_acl_hdr *ah;
	register __u16 dlen;

	if (!husb)
		return;

	DBG("%s status %d, count %d, flags %x", husb->hdev.name, urb->status, count, urb->transfer_flags);

	if (urb->status) {
		/* Do not re-submit URB on critical errors */
		switch (urb->status) {
			case -ENOENT:
				return;
			default:
				goto resubmit;
		}; // PAD DEADCODE,  must be  }  without the ';'
	}
	if (!count)
		goto resubmit;

	DMP(data, count);

	ah = (hci_acl_hdr *) data;
	dlen = le16_to_cpu(ah->dlen);

	/* Verify frame len and completeness */
	if ((count - HCI_ACL_HDR_SIZE) != dlen) {
		ERR("%s corrupted ACL packet: count %d, plen %d", husb->hdev.name, count, dlen);
		goto resubmit;
	}

	/* Allocate packet */
	if (!(skb = bluez_skb_alloc(count, GFP_ATOMIC))) {
		ERR("Can't allocate mem for new packet");
		goto resubmit;
	}

	memcpy(skb_put(skb, count), data, count);
	skb->dev = (void *) &husb->hdev;
	skb->pkt_type = HCI_ACLDATA_PKT;

	husb->hdev.stat.byte_rx += skb->len;

	hci_recv_frame(skb);

resubmit:
	husb->read_urb->dev = husb->udev;
	if ((status = usb_submit_urb(husb->read_urb)))
		DBG("%s read URB submit failed %d", husb->hdev.name, status);

	DBG("%s read URB re-submited", husb->hdev.name);
}
