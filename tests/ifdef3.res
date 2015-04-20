#include <asm/thread_info.h>
#include <asm/memory.h>
#include <asm/mach/time.h>
#include <asm/io.h>
#ifdef CONFIG_NKERNEL
#include <foo.h>
#define CONFIG_NKERNEL_NO_SHARED_IRQ	// use local (native) mask/unmask
#undef CONFIG_NKERNEL_DEBUG_IRQ
#endif
static inline void nop(void) {
	int i;
}


void init_IRQ(void)
{
	for (irq = 0; irq < IRQS; irq++) {
		*desc = irq_desc;
		uselessCall();
	}
}
