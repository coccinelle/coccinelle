#include <asm/thread_info.h>
#include <asm/memory.h>
#include <asm/mach/time.h>
#ifdef CONFIG_NKERNEL
#include <nk/nkern.h>
#include <asm/nkern.h>
unsigned long maxsize = 0;
#endif
#include <asm/io.h>


void init_IRQ(void)
{
	for (irq = 0; irq < IRQS; irq++) {
		*desc = irq_desc;
		uselessCall();
	}
}
