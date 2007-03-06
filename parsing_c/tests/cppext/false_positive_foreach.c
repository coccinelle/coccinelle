
void
ahc_plaform_send_async(struct ahc_softc *ahc, char channel,
	       u_int target, u_int lun, ac_code code, void *arg)
{
}



// even if contain loop, even if have a { then, this is not a foreach
static void loopback_enable(MGSLPC_INFO *info) 
{
	unsigned char val;

 	__asm__ __volatile__("move\t%0,$31":"=r"(ret));
	
	/* CCR1:02..00  CM[2..0] Clock Mode = 111 (clock mode 7) */ 
	val = read_reg(info, CHA + CCR1) | (BIT2 + BIT1 + BIT0);
	write_reg(info, CHA + CCR1, val);
	
	/* CCR2:04 SSEL Clock source select, 1=submode b */ 
	val = read_reg(info, CHA + CCR2) | (BIT4 + BIT5);
	write_reg(info, CHA + CCR2, val);
	
	/* set LinkSpeed if available, otherwise default to 2Mbps */ 
	if (info->params.clock_speed)
		mgslpc_set_rate(info, CHA, info->params.clock_speed);
	else
		mgslpc_set_rate(info, CHA, 1843200);
	
	/* MODE:00 TLP Test Loop, 1=loopback enabled */ 
	val = read_reg(info, CHA + MODE) | BIT0;
	write_reg(info, CHA + MODE, val);
}
