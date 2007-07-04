struct chsc_ssd_area {
	int x;
	struct chsc_header request;
	u16 :10;
	u16 ssid:2;
	u16 :4;
	u16 f_sch;	  /* first subchannel */
	u16 :16;
	u16 l_sch;	  /* last subchannel */
	u32 :32;
	struct chsc_header response;
	u32 :32;
	u8 sch_valid : 1;
	u8 dev_valid : 1;
	u8 st	     : 3; /* subchannel type */
	u8 zeroes    : 3;
	u8  unit_addr;	  /* unit address */
	u16 devno;	  /* device number */
	u8 path_mask;
	u8 fla_valid_mask;
	u16 sch;	  /* subchannel */
	u8 chpid[8];	  /* chpids 0-7 */
	u16 fla[8];	  /* full link addresses 0-7 */
};// __attribute__ ((packed));
