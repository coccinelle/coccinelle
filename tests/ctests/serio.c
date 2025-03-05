//#include <linux/serio.h>

struct serio {
	struct semaphore drv_sem;
};

static void serio_init_port(struct serio *serio)
{
	init_MUTEX(&serio->drv_sem);
}
