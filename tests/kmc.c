int
dmabounce_register_dev(struct device *dev, unsigned long small_buffer_size,
			unsigned long large_buffer_size)
{
	if (large_buffer_size) {
	  if (ret)
			goto err_destroy;
	}
	return 0;

 err_destroy:
	kfreea(device_info);
	return 1;
}
