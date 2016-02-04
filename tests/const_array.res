static const char *r128_family[] __devinitdata = {
	"AGP",
	"PCI",
};

static const char *r128_family1[] = {
	"AGP",
	"PCI",
};

static char *r128_family2[] = {
	"AGP",
	"PCI",
};

static struct foo *r128_family3[] = {
	"AGP",
	"PCI",
};

static const struct foo *r128_family4[] = {
	"AGP",
	"PCI",
};

int main () {
  ent->driver_data = ARRAY_SIZE(r128_family);
  ent->driver_data = ARRAY_SIZE(r128_family1);
  ent->driver_data = ARRAY_SIZE(r128_family2);
  ent->driver_data = ARRAY_SIZE(r128_family3);
  ent->driver_data = ARRAY_SIZE(r128_family4);
}
