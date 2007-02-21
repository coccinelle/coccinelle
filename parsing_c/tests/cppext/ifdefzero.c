void main(int i) {

#if 0
  sglurps
#endif
 
 int i;
 i++;

#if 0
	while ((loop++ < 48)&&(status=bp->r[0]->r[CD180_SRSR]&SRSR_ANYINT)){
#ifdef AURORA_INT_DEBUG
		printk("SRSR: %02x\n",status);
#endif
		ack = sbus_readb(&bp->r3->r[0]);
#ifdef AURORA_INT_DEBUG
		printk("ACK: %02x\n",ack);
#endif
		if ((ack>>5)==board_No(bp)) {
			if ((chip=((ack>>3)&3)-1) < AURORA_NCD180) {
				ack&=GSVR_ITMASK;
				if (ack==GSVR_IT_RGD) {
					aurora_receive(bp,chip);
					sbus_writeb(0,
						    &bp->r[chip]->r[CD180_EOSRR]);
				} else if (ack==GSVR_IT_REXC) {
					aurora_receive_exc(bp,chip);
					sbus_writeb(0,
						    &bp->r[chip]->r[CD180_EOSRR]);
				} else if (ack==GSVR_IT_TX) {
					aurora_transmit(bp,chip);
					sbus_writeb(0,
						    &bp->r[chip]->r[CD180_EOSRR]);
				} else if (ack==GSVR_IT_MDM) {
					aurora_check_modem(bp,chip);
					sbus_writeb(0,
						    &bp->r[chip]->r[CD180_EOSRR]);
				}
			}
		}
	}
#endif


}
