#include <asm/thread_info.h>
#include <asm/memory.h>
#include <asm/mach/time.h>
#include <asm/io.h>


void init_IRQ(void)
{
	for (irq = 0; irq < IRQS; irq++) {
#ifdef CONFIG_NKERNEL
	  if (irq < IRQ_LIMIT)
#endif  
	    *desc = irq_desc;
	  uselessCall();
	}
}
