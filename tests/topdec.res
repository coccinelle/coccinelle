#ifdef TUN_DEBUG
static int debug;
#endif

/* Network device part of the driver */

static LIST_HEAD(tun_dev_list);
static const struct ethtool_ops tun_ethtool_ops;
