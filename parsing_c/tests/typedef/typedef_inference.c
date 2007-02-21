static debug_info_t *vmcp_debug;

int i = (u32) sizeof (acpi_operand_object);


struct znet_private {
	int rx_dma, tx_dma;
	struct net_device_stats stats;
	spinlock_t lock;
	short sia_base, sia_size, io_size;
	struct i82593_conf_block i593_init;
	/* The starting, current, and end pointers for the packet buffers. */
  //HERE THERE IS TYPEDEF
 	ushort *rx_start, *rx_cur, *rx_end;
	ushort *tx_start, *tx_cur, *tx_end;
	ushort tx_buf_len;			/* Tx buffer length, in words. */
};


//parse error 
// = File "test_c/bugs/73/ok/linux-2.5.2/linux/drivers/sound/ymfpci.c", line 540, characters 76
//    around = 'ymfpci_voice_t', whole content = static int voice_alloc(ymfpci_t *codec, ymfpci_voice_type_t type, int pair, ymfpci_voice_t *rvoice[])
// charpos = 15029

static int voice_alloc(ymfpci_t *codec, ymfpci_voice_type_t type, int pair, ymfpci_voice_t *rvoice[])
{
}

