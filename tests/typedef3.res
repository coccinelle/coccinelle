typedef struct bluecard_info_t {
	struct pcmcia_device *p_dev;
} foo;

static void should_work(foo *info)
{
	unsigned int iobase = info->p_dev->io.BasePort1;
}

static void does_work(struct bluecard_info_t *info)
{
	unsigned int iobase = info->p_dev->io.BasePort1;
}
