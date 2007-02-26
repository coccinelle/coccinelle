
static struct usb_device_id storage_usb_ids [] = {

#	include "unusual_devs.h"
#undef UNUSUAL_DEV
};

static struct us_unusual_dev us_unusual_dev_list[] = {
#	include "unusual_devs.h" 

};
