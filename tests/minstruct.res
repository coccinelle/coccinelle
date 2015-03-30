static struct i2c_client client_template = {
	.dev = {
		.name = "(unset)",
	},
	.id 		= -1,
	.driver 	= &i2c_driver_videotext
};
