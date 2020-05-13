void func(struct socket *sock, struct sockaddr *uaddr, int peer)
{
	int ___addr_len;
	sock->ops->getname(sock, uaddr, &___addr_len, peer);
	return;
}
