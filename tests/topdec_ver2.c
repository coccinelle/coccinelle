#ifdef TUN_DEBUG
static int debug;
#endif

/* Network device part of the driver */

int x;
static struct ethtool_ops tun_ethtool_ops;
