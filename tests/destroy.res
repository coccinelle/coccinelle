static void wlcore_nvs_cb(const struct firmware *fw, void *context)
{
	if (ret)
		goto out_irq;

	goto out;

out_irq:
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,31)
	free_irq(wl->irq, wl);
#else
	compat_free_threaded_irq(&private->irq_compat);
	compat_destroy_threaded_irq(&wl->irq_compat);
#endif
#ifdef foo
out:
	release_firmware(fw);
#endif
}
