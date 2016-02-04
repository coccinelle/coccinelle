@header@
@@

#include <linux/etherdevice.h>

@eth_zero_addr@
expression e;
@@

-memset(e,0,ETH_ALEN);
+eth_zero_addr(e);

@eth_broadcast_addr@
identifier e;
@@

-memset(e,\(0xff\|0xFF\|255\),ETH_ALEN);
+eth_broadcast_addr(e);


@linux_header depends on !header && (eth_zero_addr || eth_broadcast_addr) @
@@

#include <linux/...>
+ #include <linux/etherdevice.h>
+


@special_header depends on !header && !linux_header && (eth_zero_addr || eth_broadcast_addr) @
@@


#include <.../...>
+
+ #include <linux/etherdevice.h>
+

@custom_header depends on !header && !linux_header && !special_header && (eth_zero_addr || eth_broadcast_addr) @
@@

#include "..."
+
+ #include <linux/etherdevice.h>
