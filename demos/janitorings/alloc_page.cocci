// From: Shani Moideen <shani.moideen@wipro.com>
// Subject: [KJ] [KJ PATCH] Replacing alloc_pages(gfp,
// 	0) with alloc_page(gfp) in net/core/sock.c
// To: jgarzik@pobox.com, akpm@linux-foundation.org
// Cc: netdev@vger.kernel.org, kernel-janitors@lists.osdl.org
// Date: Wed, 13 Jun 2007 08:16:42 +0530
// Organization: Linux COE, Wipro Technolgies
// 
// 
// Replacing alloc_pages(gfp,0) with alloc_page(gfp) 
// in net/core/sock.c
// 
// Signed-off-by: Shani Moideen <shani.moideen@wipro.com>
// ----
// 
// diff --git a/net/core/sock.c b/net/core/sock.c
// index 22183c2..25bb52b 100644
// --- a/net/core/sock.c
// +++ b/net/core/sock.c
// @@ -1193,7 +1193,7 @@ static struct sk_buff *sock_alloc_send_pskb(struct sock *sk,
//  					struct page *page;
//  					skb_frag_t *frag;
//  
// -					page = alloc_pages(sk->sk_allocation, 0);
// +					page = alloc_page(sk->sk_allocation);
//  					if (!page) {
//  						err = -ENOBUFS;
//  						skb_shinfo(skb)->nr_frags = i;

@@
expression X;
@@

- alloc_pages(X, 0)
+ alloc_page(X)