static struct i2c_client client_template = {
	.driver		= &i2c_driver_adv7175,
	.dev = {
		.name = "adv7175_client",
	}
};
