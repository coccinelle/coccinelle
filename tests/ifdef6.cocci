@ Exemple6@ @@

  #include <asm/memory.h>

+ #ifdef CONFIG_NKERNEL
+ #define foo(x) f(x)
+ #endif
