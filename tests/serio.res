#include <linux/serio.h>

static void serio_init_port(struct serio *serio)
{
	mutex_init(&serio->new_lock);
}
