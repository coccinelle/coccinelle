struct serio {
	struct mutex new_lock;
};

static void serio_init_port(struct serio *serio)
{
	mutex_init(&serio->new_lock);
}
