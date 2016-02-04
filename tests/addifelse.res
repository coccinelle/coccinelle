#if LINUX_VERSION_CODE >= KERNEL_VERSION(3,15,0)
static void rfcomm_l2data_ready(struct sock *sk)
{
	BT_DBG("%p", sk);
	rfcomm_schedule();
}
#else
static void backport_rfcomm_l2data_ready(struct sock *sk, int unused) {
	rfcomm_l2data_ready(sk);
}
#endif

static int rfcomm_l2sock_create(struct socket **sock)
{
	int err;

	BT_DBG("");

	err = sock_create_kern(PF_BLUETOOTH, SOCK_SEQPACKET, BTPROTO_L2CAP, sock);
	if (!err) {
		struct sock *sk = (*sock)->sk;
		sk->sk_data_ready   = rfcomm_l2data_ready;
		sk->sk_state_change = rfcomm_l2state_change;
	}
	return err;
}
