static void ewx_i2c_setlines(snd_i2c_bus_t *bus, int clk, int data)
{
	ice1712_t *ice = snd_magic_cast(ice1712_t, bus->private_data, return);
	unsigned char tmp = 0;
	if (clk)
		tmp |= ICE1712_EWX2496_SERIAL_CLOCK;
	if (data)
		tmp |= ICE1712_EWX2496_SERIAL_DATA;
	snd_ice1712_write(ice, ICE1712_IREG_GPIO_DATA, tmp);
	udelay(5);
}
