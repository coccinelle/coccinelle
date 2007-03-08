struct speedo_mc_block {
	struct speedo_mc_block *next;
	unsigned int tx;
	dma_addr_t frame_dma;
	unsigned int len;
 	struct descriptor frame __attribute__ ((__aligned__(16)));
};
