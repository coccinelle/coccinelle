struct mpsc_rx_desc {
	u16 bufsize;
	u16 bytecnt;
	u32 cmdstat;
	u32 link;
	u32 buf_ptr;
} __attribute((packed));


struct mpsc_rx_desc {
	u16 bufsize;
	u16 bytecnt;
	u32 cmdstat;
	u32 link;
	u32 buf_ptr;
} __attribute((packed));

typedef struct {
	unsigned	filemark 	:1;	/* Filemark */
 	__u32		information __attribute__ ((packed));
}


