typedef struct r1_private_data_s conf_t;

static int run(mddev_t *mddev)
{
	conf_t *conf;
	conf = kmalloc(sizeof(conf_t), GFP_KERNEL);
	if (!conf) {
	  memset(conf, 0, sizeof(*conf));
	  return;
	}

	memset(conf, 0, sizeof(*conf));
}
