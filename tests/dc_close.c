void __init
initicc(struct IsdnCardState *cs)
{
	int val, eval;

	INIT_WORK(&cs->work, icc_bh, cs);
	cs->setstack_d = setstack_icc;
	cs->DC_Close = DC_Close_icc;
	cs->dc.icc.mon_rx = NULL;

	if (val & 0x01) {
		eval = cs->readisac(cs, ICC_EXIR);
	}
	val = cs->readisac(cs, ICC_CIR0);
  	cs->dc.icc.mocr = 0xaa;
	if (test_bit(HW_IOM1, &cs->HW_Flags)) {
		/* IOM 1 Mode */
		cs->writeisac(cs, ICC_ADF2, 0x0);
	} else {
		/* IOM 2 Mode */
		if (!cs->dc.icc.adf2)
			cs->dc.icc.adf2 = 0x80;
		cs->writeisac(cs, ICC_ADF2, cs->dc.icc.adf2);
	}
	ph_command(cs, ICC_CMD_RES);
}
