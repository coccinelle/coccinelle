@@
expression E;
@@

- printk("... %s ...",
+ printk("... %u.%u.%u.%u ...",
          ...,
-         in_ntoa(E),
+         NIPQUAD(E),
          ...);
