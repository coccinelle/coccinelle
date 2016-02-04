static void rfcomm_l2data_ready(struct sock *sk)
{
	BT_DBG("%p", sk);
	rfcomm_schedule();
}

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
