static CLASS_DEVICE_ATTR(reg, S_IRUGO | S_IWUSR,
                         sn9c102_show_reg, sn9c102_store_reg);

static DEVICE_ATTR(sensor1_temperature,	S_IRUGO,
		   show_sensor1_temperature,NULL);

static DRIVER_ATTR(rescan,
	S_IRUGO | S_IWUSR, hvcs_rescan_show, hvcs_rescan_store);


static DRIVER_ATTR(rescan,
	S_IRUGO | S_IWUSR, hvcs_rescan_show, hvcs_rescan_store);

