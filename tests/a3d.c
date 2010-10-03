struct a3d {
	struct gameport adc;
	struct input_dev dev;
};

static void a3d_connect(struct gameport *gameport, struct gameport_dev *dev)
{
	struct a3d *a3d;
	a3d->adc.idbus = BUS_GAMEPORT;
	a3d->dev.idbus = BUS_GAMEPORT;
}

static void a3d_connect(struct gameport *gameport, struct gameport_dev *dev)
{
	struct a3d *a3d;
	a3d->adc.idbus = BUS_GAMEPORT;
}

