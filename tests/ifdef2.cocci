@ Exemple2 @  @@

  #include <asm/mach/time.h>
+ #ifdef CONFIG_NKERNEL
+ #include <nk/nkern.h>
+ #include <asm/nkern.h>
+ unsigned long maxsize = 0;
+ #endif
