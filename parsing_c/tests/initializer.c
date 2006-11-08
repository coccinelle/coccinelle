static struct block_device_operations gscd_fops = {
	.owner		= THIS_MODULE,
	.open		= gscd_open,
	.release	= gscd_release,
	.ioctl		= gscd_ioctl,
	.media_changed	= check_gscd_med_chg,
};

static struct request_queue *gscd_queue;


static struct request_queue *gscd_queue;
