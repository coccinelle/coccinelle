struct device {
	struct netdev_private *priv;
	struct netdev_private2 *priv2;
	void *priv3;
	

};

void f(struct device *dev) 
{

	struct netdev_private *np = 
		dev->priv;

	struct netdev_private *np2 = 
		dev->priv2;

	struct netdev_private *np3 = 
		dev->priv3;

}
