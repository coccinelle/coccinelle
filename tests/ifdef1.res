#include <asm/thread_info.h>
#include <asm/memory.h>
#ifdef CONFIG_NKERNEL
#include <asm/nk/f_nk.h>
#endif
#include <asm/mach/time.h>
#include <asm/io.h>


void init_IRQ(void)
{
	for (irq = 0; irq < IRQS; irq++) {
		*desc = irq_desc;
		uselessCall();
	}
}
