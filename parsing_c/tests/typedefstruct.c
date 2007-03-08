//typedef int card_t;

typedef struct {
	struct net_device *dev;
	struct card_t *card;
	spinlock_t lock;	/* for wanxl_xmit */
        int node;		/* physical port #0 - 3 */
	unsigned int clock_type;
	int tx_in, tx_out;
	struct sk_buff *tx_skbs[TX_BUFFERS];
}port_t;


typedef struct {
	desc_t rx_descs[RX_QUEUE_LENGTH];
	port_status_t port_status[4];
}card_status_t;


typedef struct card_t {
	int n_ports;		/* 1, 2 or 4 ports */
	u8 irq;

	u8 __iomem *plx;	/* PLX PCI9060 virtual base address */
	struct pci_dev *pdev;	/* for pci_name(pdev) */
	int rx_in;
	struct sk_buff *rx_skbs[RX_QUEUE_LENGTH];
	card_status_t *status;	/* shared between host and card */
	dma_addr_t status_address;
	port_t ports[0];	/* 1 - 4 port_t structures follow */
}card_t;


