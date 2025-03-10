static void tc574_config(dev_link_t *link);

static int tc574_attach(struct pcmcia_device *p_dev)
{
	dev_link_t *link = dev_to_instance(p_dev);
}

static void tc574_detach(struct pcmcia_device *p_dev)
{
	dev_link_t *link = dev_to_instance(p_dev);
}
