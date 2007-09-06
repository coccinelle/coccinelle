void __init
initicc(struct IsdnCardState *cs)
{
	int val, eval;

	INIT_WORK(&cs->work, icc_bh, cs);
	cs->setstack_d = setstack_icc;
	cs->DC_Close = DC_Close_icc;
	cs->dc.icc.mon_tx = NULL;
	cs->dc.icc.mon_rx = NULL;
	cs->dbusytimer.function = (void *) dbusy_timer_handler;
	cs->dbusytimer.data = (long) cs;
	init_timer(&cs->dbusytimer);

	val = cs->readisac(cs, ICC_STAR);
	debugl1(cs, "ICC STAR %x", val);
	val = cs->readisac(cs, ICC_MODE);
	debugl1(cs, "ICC MODE %x", val);
	val = cs->readisac(cs, ICC_ADF2);
	debugl1(cs, "ICC ADF2 %x", val);
	val = cs->readisac(cs, ICC_ISTA);
	debugl1(cs, "ICC ISTA %x", val);
	if (val & 0x01) {
		eval = cs->readisac(cs, ICC_EXIR);
		debugl1(cs, "ICC EXIR %x", eval);
	}
	val = cs->readisac(cs, ICC_CIR0);
	debugl1(cs, "ICC CIR0 %x", val);
	cs->dc.icc.ph_state = (val >> 2) & 0xf;
	sched_d_event(cs, D_L1STATECHANGE);
	/* Disable all IRQ */
	cs->writeisac(cs, ICC_MASK, 0xFF);

  	cs->dc.icc.mocr = 0xaa;
	if (test_bit(HW_IOM1, &cs->HW_Flags)) {
		/* IOM 1 Mode */
		cs->writeisac(cs, ICC_ADF2, 0x0);
		cs->writeisac(cs, ICC_SPCR, 0xa);
		cs->writeisac(cs, ICC_ADF1, 0x2);
		cs->writeisac(cs, ICC_STCR, 0x70);
		cs->writeisac(cs, ICC_MODE, 0xc9);
	} else {
		/* IOM 2 Mode */
		if (!cs->dc.icc.adf2)
			cs->dc.icc.adf2 = 0x80;
		cs->writeisac(cs, ICC_ADF2, cs->dc.icc.adf2);
		cs->writeisac(cs, ICC_SQXR, 0xa0);
		cs->writeisac(cs, ICC_SPCR, 0x20);
		cs->writeisac(cs, ICC_STCR, 0x70);
		cs->writeisac(cs, ICC_MODE, 0xca);
		cs->writeisac(cs, ICC_TIMR, 0x00);
		cs->writeisac(cs, ICC_ADF1, 0x20);
	}
	ph_command(cs, ICC_CMD_RES);
	cs->writeisac(cs, ICC_MASK, 0x0);
	ph_command(cs, ICC_CMD_DI);
}
