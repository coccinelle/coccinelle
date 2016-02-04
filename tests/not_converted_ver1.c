// -ifdef_to_if doesn't convert this ifdef

static int do_accept(int acc_sock, int *sock, char **host)
	{
	int ret,i;
	struct hostent *h1,*h2;
	static struct sockaddr_in from;
	int len;
/*	struct linger ling; */

	if (!ssl_sock_init()) return(0);

#ifndef OPENSSL_SYS_WINDOWS
redoit:
#endif

	memset((char *)&from,0,sizeof(from));
	len=sizeof(from);
	/* Note: under VMS with SOCKETSHR the fourth parameter is currently
	 * of type (int *) whereas under other systems it is (void *) if
	 * you don't have a cast it will choke the compiler: if you do
	 * have a cast then you can either go for (int *) or (void *).
	 */
	ret=accept(acc_sock,(struct sockaddr *)&from,(void *)&len);
	if (ret == INVALID_SOCKET)
		{
#if defined(OPENSSL_SYS_WINDOWS) || (defined(OPENSSL_SYS_NETWARE) && !defined(NETWARE_BSDSOCK))
		i=WSAGetLastError();
		BIO_printf(bio_err,"accept error %d\n",i);
#else
		if (errno == EINTR)
			{
			/*check_timeout(); */
			goto redoit;
			}
		fprintf(stderr,"errno=%d ",errno);
		perror("accept");
#endif
		return(0);
		}

/*
	ling.l_onoff=1;
	ling.l_linger=0;
	i=setsockopt(ret,SOL_SOCKET,SO_LINGER,(char *)&ling,sizeof(ling));
	if (i < 0) { perror("linger"); return(0); }
	i=0;
	i=setsockopt(ret,SOL_SOCKET,SO_KEEPALIVE,(char *)&i,sizeof(i));
	if (i < 0) { perror("keepalive"); return(0); }
*/

	if (host == NULL) goto end;
#ifndef BIT_FIELD_LIMITS
	/* I should use WSAAsyncGetHostByName() under windows */
	h1=gethostbyaddr((char *)&from.sin_addr.s_addr,
		sizeof(from.sin_addr.s_addr),AF_INET);
#else
	h1=gethostbyaddr((char *)&from.sin_addr,
		sizeof(struct in_addr),AF_INET);
#endif
	if (h1 == NULL)
		{
		BIO_printf(bio_err,"bad gethostbyaddr\n");
		*host=NULL;
		/* return(0); */
		}
	else
		{
		if ((*host=(char *)OPENSSL_malloc(strlen(h1->h_name)+1)) == NULL)
			{
			perror("OPENSSL_malloc");
			return(0);
			}
		BUF_strlcpy(*host,h1->h_name,strlen(h1->h_name)+1);

		h2=GetHostByName(*host);
		if (h2 == NULL)
			{
			BIO_printf(bio_err,"gethostbyname failure\n");
			return(0);
			}
		i=0;
		if (h2->h_addrtype != AF_INET)
			{
			BIO_printf(bio_err,"gethostbyname addr is not AF_INET\n");
			return(0);
			}
		}
end:
	*sock=ret;
	return(1);
	}
