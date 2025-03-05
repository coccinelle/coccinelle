@ Exemple1@ @@

  #include <asm/memory.h>

+ #ifdef CONFIG_NKERNEL
+ #include <asm/nk/f_nk.h>
+ #endif
