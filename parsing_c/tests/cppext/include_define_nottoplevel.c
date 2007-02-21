static struct usb_eth_dev usb_dev_id[] = {
#define	PEGASUS_DEV(pn, vid, pid, flags)	\
	{name:pn, vendor:vid, device:pid, private:flags},
#include "pegasus.h"
#undef	PEGASUS_DEV
	{NULL, 0, 0, 0}
};



static u32 __initdata ucode_start = 
#include UCODE(start)
;



#ifdef AMB_NEW_MICROCODE
#define UCODE(x) UCODE2(atmsar12.x)
#else
#define UCODE(x) UCODE2(atmsar11.x)
#endif
#define UCODE2(x) #x

static u32 __devinitdata ucode_start =
#include UCODE(start)
;

static region __devinitdata ucode_regions[] = {
#include UCODE(regions)
  { 0, 0 }
};

static u32 __devinitdata ucode_data[] = {
#include UCODE(data)
  0xdeadbeef
};
