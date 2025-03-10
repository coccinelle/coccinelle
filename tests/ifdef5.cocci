@ Exemple5 @  @@

  init_IRQ(...) {...}

+ #ifdef CONFIG_NKERNEL
+ #ifndef TIMER_32K_SYNCHRONIZED
+ #define TIMER_32K_SYNCHRONIZED	0xffffffff
+ #endif

+ unsigned long nk_vtick_read_stamp(void)
+ {
+ return omap_readl(TIMER_32K_SYNCHRONIZED);
+ }
