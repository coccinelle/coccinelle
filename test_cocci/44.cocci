@@
expression E;
@@

- printk("... %s ...", ..., in_ntoa(E), ...);
+ printk("... %u.%u.%u.%u ...", ..., NIPQUAD(E), ...);
