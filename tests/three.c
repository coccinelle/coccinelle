int ide_event(event_t event, int priority,
	      event_callback_args_t *args)
{
    dev_link_t *link = args->client_data;

    DEBUG(1, "ide_event(0x%06x)\n", event);
} /* ide_event */

/*====================================================================*/

static int init_ide_cs(void)
{
    register_pccard_driver(&ide_attach);
    return 0;
}

static void exit_ide_cs(void)
{
    unregister_pccard_driver(&dev_info);
}
