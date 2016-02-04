
// from: http://kernelnewbies.org/KernelJanitors/Todo 
struct device {
	struct netdev_private *priv;
	struct netdev_private2 *priv2;
	void *priv3;

};

struct net_device *dev;

struct netdev_private *np = (struct netdev_private *)dev->priv;
struct netdev_private *np2 = (struct netdev_private *)dev->priv2;
struct netdev_private *np3 = (struct netdev_private *)dev->priv3;


struct netdev_private *np4 = (struct netdev_private *) dev;


int f(int i)
{
	ssize_t x;
	if((ssize_t) x > 0) {
		return -1;
	}
}
