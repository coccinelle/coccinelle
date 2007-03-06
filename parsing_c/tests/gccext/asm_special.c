
void main(int i) {
	register unsigned long func asm("%o5");

 	register unsigned long dma_cnt asm ("d3");

 	register u32  _crc __asm ( "ax" );
}
