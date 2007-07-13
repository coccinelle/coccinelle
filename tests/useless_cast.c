
// from: http://kernelnewbies.org/KernelJanitors/Todo 
struct device {
	struct netdev_private *priv;
	struct netdev_private2 *priv2;

};

struct device *dev;

struct netdev_private *np3 = (struct netdev_private *) 
   dev;
