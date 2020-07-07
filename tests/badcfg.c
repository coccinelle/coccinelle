const char *netdev_cmd_to_name(enum netdev_cmd cmd)
{
#define N(val) 						\
	case NETDEV_##val:				\
		return "NETDEV_" __stringify(val);
	switch (cmd) {
	N(UP) N(DOWN)
	N(PRE_CHANGEADDR)
	}
#undef N
	return "UNKNOWN_NETDEV_EVENT";
}
