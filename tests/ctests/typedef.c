typedef struct bluecard_info_t {
	dev_link_t link;
} foo;

static void should_work(u_long arg)
{
	foo *info = (struct bluecard_info_t *)arg;
	unsigned int iobase = info->link.io.BasePort1;
}

static void does_work(u_long arg)
{
	struct bluecard_info_t *info = (struct bluecard_info_t *)arg;
	unsigned int iobase = info->link.io.BasePort1;
}
