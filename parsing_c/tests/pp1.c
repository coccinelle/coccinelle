/* queue a command */
/* This is always called with scsi_lock(srb->host) held */

	/* fixup tx packet (add framing) */
struct sk_buff	*(*tx_fixup)(struct usbnet *dev, struct sk_buff *skb, int flags);

static inline int rc_request_io_range(struct riscom_board * const bp)
{
}

volatile extern void *_periph_base;


static int a[10][20]; /* 8 targets and 8 LUNs */
static int *b[2];

static int nsp32_probe(struct pci_dev *, const struct pci_device_id *);



//{{    // just to produce error and test the  NotParsedCorrectly handling code

struct toto {
	int	bus, bus2;
	u_char	sv_scntl0, sv_scntl3, sv_dmode, sv_dcntl, sv_ctest3, sv_ctest4,
		sv_ctest5, sv_gpcntl, sv_stest2, sv_stest4, sv_stest1, sv_scntl4;
	int	bus, bus2;
  /*	u_char	rv_scntl0, rv_scntl3, rv_dmode, rv_dcntl, rv_ctest3, rv_ctest4, 
        rv_ctest5, rv_stest2, rv_ccntl0, rv_ccntl1, rv_scntl4;*/
};


typedef struct {
	struct tcb	target[MAX_TARGET];
	int	bus, bus2;
	volatile struct ncr_reg	*reg;
} ncr_slot;


