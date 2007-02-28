static void xm_link_timer(struct work_struct *work)
{
	struct net_device *dev = arg;
	struct skge_port *skge = netdev_priv(arg);
 	struct skge_hw *hw = skge->hw;
	int port = skge->port;

	if (!netif_running(dev))
		return;

	if (netif_carrier_ok(dev)) {
		xm_read16(hw, port, XM_ISRC);
		if (!(xm_read16(hw, port, XM_ISRC) & XM_IS_INP_ASS))
			goto nochange;
	} 

nochange:
	schedule_delayed_work(&skge->link_thread, LINK_HZ);
}
