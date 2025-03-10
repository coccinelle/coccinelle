static int hexium_attach(struct saa7146_dev *dev, struct saa7146_pci_extension_data *info)
{
	struct hexium *hexium;
	int ret;

	DEB_EE("\n");

	hexium = kzalloc(sizeof(struct hexium), GFP_KERNEL);
	hexium->i2c_adapter = (struct i2c_adapter) {
		.name = "hexium gemini",
	};
	saa7146_i2c_adapter_prepare(dev, &hexium->i2c_adapter, SAA7146_I2C_BUS_BIT_RATE_480);
}
