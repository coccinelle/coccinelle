static void elsa_cs_detach(struct pcmcia_device *link)
{

} /* elsa_cs_detach */

static void elsa_cs_config(struct pcmcia_device *link)
{
    int i, j, last_fn;
}

static struct pcmcia_driver elsa_cs_driver = {
  .detach = elsa_cs_detach
};
