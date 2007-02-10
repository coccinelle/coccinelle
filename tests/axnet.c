static void axnet_config(struct pcmcia_device *link)
{
    if (last_ret != CS_SUCCESS) {
	cs_error(link, RequestIO, last_ret);
	goto failed;
    }
    return;

cs_failed:
    cs_error(link, last_fn, last_ret);
failed:
    axnet_release(link);
    link->state &= ~DEV_CONFIG_PENDING;
    return;
}
