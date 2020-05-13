void func(struct socket *sock, struct sockaddr *uaddr, int peer)
{
	sock->ops->getname(sock, uaddr, peer);
	return;
}
