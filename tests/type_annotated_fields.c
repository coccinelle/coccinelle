typedef struct bluecard_info_t {
	dev_link_t link;
	dev_node_t node;

	struct hci_dev *hdev;

	spinlock_t lock;		/* For serializing operations */
	struct timer_list timer;	/* For LED control */

	struct sk_buff_head txq;
	unsigned long tx_state;

	unsigned long rx_state;
	unsigned long rx_count;
	struct sk_buff *rx_skb;

	unsigned char ctrl_reg;
	unsigned long hw_state;		/* Status of the hardware and LED control */
} bluecard_info_t;


static int bluecard_hci_send_frame(struct sk_buff *skb)
{
	bluecard_info_t *info;
	struct hci_dev *hdev = (struct hci_dev *)(skb->dev);
        skb->pkt_type;
        hdev->stat.cmd_tx++;
}

static void bluecard_receive(bluecard_info_t *info, unsigned int offset)
{

	unsigned char buf[31];
	int i, len;
        bluecard_info_t info2;

        info->rx_skb->pkt_type = buf[i];
        info2.tx_state;
        
}


