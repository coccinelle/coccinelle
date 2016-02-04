typedef struct bluecard_info_t {
	dev_link_t link;
} foo;

static void should_work(foo *info)
{
	unsigned int iobase = info->link.io.BasePort1;
}

static void does_work(struct bluecard_info_t *info)
{
	unsigned int iobase = info->link.io.BasePort1;
}
