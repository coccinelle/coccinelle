typedef struct bluecard_info_t {
	struct pcmcia_device *p_dev;
} foo;

static void should_work(u_long arg)
{
	foo *info = (struct bluecard_info_t *)arg;
	unsigned int iobase = info->p_dev->io.BasePort1;
}

static void does_work(u_long arg)
{
	struct bluecard_info_t *info = (struct bluecard_info_t *)arg;
	unsigned int iobase = info->p_dev->io.BasePort1;
}
