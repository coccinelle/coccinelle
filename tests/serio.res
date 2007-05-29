#include <linux/serio.h>

static void serio_init_port(struct serio *serio)
{
	init_MUTEX(&serio->new_lock);
}
