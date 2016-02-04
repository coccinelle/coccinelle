#include <asm/thread_info.h>
#ifdef CONFIG_NKERNEL
#define foo(x) f(x)
#endif
#include <asm/memory.h>
#include <asm/mach/time.h>
#include <asm/io.h>


void init_IRQ(void)
{
	for (irq = 0; irq < IRQS; irq++) {
		*desc = irq_desc;
		uselessCall();
	}
}
