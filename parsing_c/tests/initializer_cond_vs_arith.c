
static int ttusb_update_lnb(struct ttusb *ttusb)
{

  int x = 		ttusb->voltage == SEC_VOLTAGE_18 ? 0 : 1;


	u8 b[] = { 0xaa, ++ttusb->c, 0x16, 5, /*power: */ 1,
 		ttusb->voltage == SEC_VOLTAGE_18 ? 0 : 1,
		ttusb->tone == SEC_TONE_ON ? 1 : 0, 1, 1
	};
}

