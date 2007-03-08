u32 script[sizeof(SCRIPT)/4] __attribute__ ((aligned (4)));

typedef struct {
	unsigned	error_code	:7;	/* Current of deferred errors */
	unsigned	valid		:1;	/* The information field conforms to QIC-157C */
	__u8		reserved1	:8;	/* Segment Number - Reserved */
	unsigned	sense_key	:4;	/* Sense Key */
	unsigned	reserved2_4	:1;	/* Reserved */
	unsigned	ili		:1;	/* Incorrect Length Indicator */
	unsigned	eom		:1;	/* End Of Medium */
	unsigned	filemark 	:1;	/* Filemark */
 	__u32		information __attribute__ ((packed));
	__u8		asl;			/* Additional sense length (n-7) */
	__u32		command_specific;	/* Additional command specific information */
	__u8		asc;			/* Additional Sense Code */
	__u8		ascq;			/* Additional Sense Code Qualifier */
	__u8		replaceable_unit_code;	/* Field Replaceable Unit Code */
	unsigned	sk_specific1 	:7;	/* Sense Key Specific */
	unsigned	sksv		:1;	/* Sense Key Specific information is valid */
	__u8		sk_specific2;		/* Sense Key Specific */
	__u8		sk_specific3;		/* Sense Key Specific */
	__u8		pad[2];			/* Padding to 20 bytes */
} idetape_request_sense_result_t;



struct ns83820 {
	struct net_device_stats	stats;
	u8			__iomem *base;

	struct pci_dev		*pci_dev;

#ifdef NS83820_VLAN_ACCEL_SUPPORT
	struct vlan_group	*vlgrp;
#endif

	struct rx_info		rx_info;
	struct tasklet_struct	rx_tasklet;

	unsigned		ihr;
	struct work_struct	tq_refill;

	/* protects everything below.  irqsave when using. */
	spinlock_t		misc_lock;

	u32			CFG_cache;

	u32			MEAR_cache;
	u32			IMR_cache;

	unsigned		linkstate;

	spinlock_t	tx_lock;

	u16		tx_done_idx;
	u16		tx_idx;
	volatile u16	tx_free_idx;	/* idx of free desc chain */
	u16		tx_intr_idx;

	atomic_t	nr_tx_skbs;
	struct sk_buff	*tx_skbs[NR_TX_DESC];

 	char		pad[16] __attribute__((aligned(16)));
	u32		*tx_descs;
	dma_addr_t	tx_phy_descs;

	struct timer_list	tx_watchdog;
};

