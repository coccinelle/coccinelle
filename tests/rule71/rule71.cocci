@@
struct scatterlist *sg;
expression i;
@@

#include "usb.h"

- page_address(sg[i].page) + sg[i].offset
+ sg_address(sg[i])
