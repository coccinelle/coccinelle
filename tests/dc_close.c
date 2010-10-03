void __init
initicc(struct IsdnCardState *cs)
{
	int val, eval;

	INIT_WORK(&cs->work, icc_bh, cs);
	cs->setstack_d = setstack_icc;
	cs->DC_Close = DC_Close_icc;
	cs->dc.icc.mon_rx = NULL;

}
