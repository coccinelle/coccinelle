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
  ent->driver_data = sizeof(r128_family)/sizeof(char *);
  ent->driver_data = sizeof(r128_family1)/sizeof(char *);
  ent->driver_data = sizeof(r128_family2)/sizeof(char *);
  ent->driver_data = sizeof(r128_family3)/sizeof(struct foo *);
  ent->driver_data = sizeof(r128_family4)/sizeof(struct foo *);
}
