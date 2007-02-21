static int __init ncr_regtest (struct ncb* np)
{
	register volatile u_int32 data;
	/*
	**	ncr registers may NOT be cached.
	**	write 0xffffffff to a read only register area,
	**	and try to read it back.
	*/
	data = 0xffffffff;
	OUTL_OFF(offsetof(struct ncr_reg, nc_dstat), data);
	data = INL_OFF(offsetof(struct ncr_reg, nc_dstat));
#if 1
	if (data == 0xffffffff) {
#else
	if ((data & 0xe2f0fffd) != 0x02000080) {
#endif
		printk ("CACHE TEST FAILED: reg dstat-sstat2 readback %x.\n",
			(unsigned) data);
		return (0x10);
	};
	return (0);
}
#endif

static int __init ncr_snooptest (struct ncb* np)
{
	u_int32	ncr_rd, ncr_wr, ncr_bk, host_rd, host_wr, pc;
	u_char  dstat;
	int	i, err=0;


#ifdef SL_INCLUDE_CSLIP
	if (xbuff == NULL || rbuff == NULL || cbuff == NULL)  {
#else
	if (xbuff == NULL || rbuff == NULL)  {
#endif

          foo();
        }
}

//parse error 
// = File "test_c/bugs/9/ok/linux-2.5.70/drivers/scsi/ncr53c8xx.c", line 8361, characters 0
//    around = '{', whole content = {
// charpos = 208641
//FOUND SYNC at line 8441
//pad: a cause de ifdef,  qui fait que y'a plus de if que de raison et donc plus de } de prevu ?
