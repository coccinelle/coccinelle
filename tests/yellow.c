// search for "paren missing"

static void yellowfin_init_ring(struct net_device *dev)
{
	struct yellowfin_private *yp = netdev_priv(dev);
	int i;

	yp->tx_full = 0;
	yp->cur_rx = yp->cur_tx = 0;
	yp->dirty_tx = 0;

	yp->rx_buf_sz = (dev->mtu <= 1500 ? PKT_BUF_SZ : dev->mtu + 32);

	for (i = 0; i < RX_RING_SIZE; i++) {
		yp->rx_ring[i].dbdma_cmd =
			cpu_to_le32(CMD_RX_BUF | INTR_ALWAYS | yp->rx_buf_sz);
		yp->rx_ring[i].branch_addr = cpu_to_le32(yp->rx_ring_dma +
			((i+1)%RX_RING_SIZE)*sizeof(struct yellowfin_desc));
	}

	for (i = 0; i < RX_RING_SIZE; i++) {
		struct sk_buff *skb = dev_alloc_skb(yp->rx_buf_sz);
		yp->rx_skbuff[i] = skb;
		if (skb == NULL)
			break;
		skb->dev = dev;		/* Mark as being used by this device. */
		skb_reserve(skb, 2);	/* 16 byte align the IP header. */
		yp->rx_ring[i].addr = cpu_to_le32(pci_map_single(yp->pci_dev,
			skb->data, yp->rx_buf_sz, PCI_DMA_FROMDEVICE));
	}
	yp->rx_ring[i-1].dbdma_cmd = cpu_to_le32(CMD_STOP);
	yp->dirty_rx = (unsigned int)(i - RX_RING_SIZE);

#define NO_TXSTATS
#ifdef NO_TXSTATS
	/* In this mode the Tx ring needs only a single descriptor. */
	for (i = 0; i < TX_RING_SIZE; i++) {
		yp->tx_skbuff[i] = NULL;
		yp->tx_ring[i].dbdma_cmd = cpu_to_le32(CMD_STOP);
		yp->tx_ring[i].branch_addr = cpu_to_le32(yp->tx_ring_dma +
			((i+1)%TX_RING_SIZE)*sizeof(struct yellowfin_desc));
	}
	/* Wrap ring */
	yp->tx_ring[--i].dbdma_cmd = cpu_to_le32(CMD_STOP | BRANCH_ALWAYS);
#else
{
	int j;

	/* Tx ring needs a pair of descriptors, the second for the status. */
	for (i = 0; i < TX_RING_SIZE; i++) {
		j = 2*i;
		yp->tx_skbuff[i] = 0;
		/* Branch on Tx error. */
		yp->tx_ring[j].dbdma_cmd = cpu_to_le32(CMD_STOP);
		yp->tx_ring[j].branch_addr = cpu_to_le32(yp->tx_ring_dma +
			(j+1)*sizeof(struct yellowfin_desc);//paren missing
		j++;
		if (yp->flags & FullTxStatus) {
			yp->tx_ring[j].dbdma_cmd =
				cpu_to_le32(CMD_TXSTATUS | sizeof(*yp->tx_status));
			yp->tx_ring[j].request_cnt = sizeof(*yp->tx_status);
			yp->tx_ring[j].addr = cpu_to_le32(yp->tx_status_dma +
				i*sizeof(struct tx_status_words);//paren missing
		} else {
			/* Symbios chips write only tx_errs word. */
			yp->tx_ring[j].dbdma_cmd =
				cpu_to_le32(CMD_TXSTATUS | INTR_ALWAYS | 2);
			yp->tx_ring[j].request_cnt = 2;
			/* Om pade ummmmm... */
			yp->tx_ring[j].addr = cpu_to_le32(yp->tx_status_dma +
				i*sizeof(struct tx_status_words) +
				&(yp->tx_status[0].tx_errs) - 
				&(yp->tx_status[0]));
		}
		yp->tx_ring[j].branch_addr = cpu_to_le32(yp->tx_ring_dma + 
			((j+1)%(2*TX_RING_SIZE))*sizeof(struct yellowfin_desc));
	}
	/* Wrap ring */
	yp->tx_ring[++j].dbdma_cmd |= cpu_to_le32(BRANCH_ALWAYS | INTR_ALWAYS);
}
#endif
	yp->tx_tail_desc = &yp->tx_status[0];
	return;
}

int foo () 
{ 
return; 
}
