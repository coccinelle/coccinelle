struct net_device *alloc_etherdev(int sizeof_priv)
{
         return alloc_netdev(sizeof_priv, "eth%d", ether_setup);
}
