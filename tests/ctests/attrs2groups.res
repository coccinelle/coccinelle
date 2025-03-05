static ssize_t devspec_show(struct device *dev,
				struct device_attribute *attr, char *buf)
{
	return 42;
}
static BUG();
NOBUG();

static ssize_t name_show(struct device *dev,
				struct device_attribute *attr, char *buf)
{
	return 0;
}
static BUG();
NOBUG();

static ssize_t modalias_show(struct device *dev,
				struct device_attribute *attr, char *buf)
{
	return 1;
}
static BUG();
NOBUG();


struct device_attribute ibmebus_bus_device_attrs[] = {
	__ATTR_RO(devspec),
	__ATTR_RO(name),
	__ATTR_RO(modalias),
	__ATTR_NULL
};

struct bus_type ibmebus_bus_type = {
	.dev_attrs = ibmebus_bus_device_attrs,
};
