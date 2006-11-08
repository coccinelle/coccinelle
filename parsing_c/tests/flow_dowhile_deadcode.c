static inline u16 NFTL_findwriteunit(struct NFTLrecord *nftl, unsigned block)
{
	do {
		if (writeEUN == BLOCK_NIL) {
			lastEUN = BLOCK_NIL;
			//break;
                        continue;
		}
		return writeEUN;

	} while (silly2--);

	return 0xffff;
}
