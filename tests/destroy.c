static void wlcore_nvs_cb(const struct firmware *fw, void *context)
{
	if (ret)
		goto out_irq;

	goto out;

out_irq:
	free_irq(wl->irq, wl);
#ifdef foo
out:
	release_firmware(fw);
#endif
}
