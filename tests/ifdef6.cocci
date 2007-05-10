@ Exemple6@ @@

  #include <asm/memory.h>

+ #ifdef CONFIG_NKERNEL
+ #define foo <asm/nk/f_nk.h>
+ #endif
