#include <asm/thread_info.h>
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
#ifdef CONFIG_NKERNEL
#ifndef TIMER_32K_SYNCHRONIZED
#define TIMER_32K_SYNCHRONIZED 0xffffffff
#endif
unsigned long nk_vtick_read_stamp(void) {
	return omap_readl(TIMER_32K_SYNCHRONIZED);
}
