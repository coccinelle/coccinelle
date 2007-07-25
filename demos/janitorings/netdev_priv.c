void foo(struct net_device *dev)
{
	struct priv *priv = dev->priv;
}

void main(void)
{
	struct priv *priv;
	struct net_device *dev;
//	dev = alloc_netdev(sizeof(struct priv), 0);
	dev = alloc_netdev(sizeof(*priv), 0);

	
}
