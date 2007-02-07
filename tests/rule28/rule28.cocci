// Even the semantic patch isn't going to parse, because of the calls to
// printk.  These could easily become ...; then just the C file has to
// address the problem.

@@
struct BCState *X;
//expression Z1, Z2;
@@

- if (!test_and_set_bit(BC_FLG_INIT, &X->Flag)) {
-   if (!(X->rcvbuf = kmalloc(HSCX_BUFMAX, GFP_ATOMIC))) {
-     printk(...); // was KERN_WARNING Z1
(
?-    test_and_clear_bit(BC_FLG_INIT, &X->Flag);
|
?-    clear_bit(BC_FLG_INIT, &X->Flag);
)
-     return (1);
-   }
-   if (!(X->blog = kmalloc(MAX_BLOG_SPACE, GFP_ATOMIC))) {
-     printk(...); // was KERN_WARNING Z2
(
-     test_and_clear_bit(BC_FLG_INIT, &X->Flag);
|
-     clear_bit(BC_FLG_INIT, &X->Flag);
)
-     kfree(X->rcvbuf);
-     X->rcvbuf = NULL;
-     return (2);
-   }
?-  skb_queue_head_init(&X->rqueue);
?-  skb_queue_head_init(&X->squeue);
?-  skb_queue_head_init(&X->cmpl_queue);
- }
- X->tx_skb = NULL;
(
- test_and_clear_bit(BC_FLG_BUSY, &X->Flag);
|
- clear_bit(BC_FLG_BUSY, &X->Flag);
)
- X->event = 0;
- X->rcvidx = 0;
- X->tx_cnt = 0;
- return (0);
+ return bc_open(X);

@@
struct BCState *X;
@@

- if (
(
-     test_and_clear_bit(BC_FLG_INIT, &X->Flag)
|
-     clear_bit(BC_FLG_INIT, &X->Flag)
)
-    ) {
-   if (X->rcvbuf) {
-     kfree(X->rcvbuf);
-     X->rcvbuf = NULL;
-   }
-   if (X->blog) {
-     kfree(X->blog);
-     X->blog = NULL;
-   }
?-  skb_queue_purge(&X->rqueue);
?-  skb_queue_purge(&X->squeue);
?-  skb_queue_purge(&X->cmpl_queue);
-   if (X->tx_skb) {
-     dev_kfree_skb_any(X->tx_skb);
-     X->tx_skb = NULL;
(
-     test_and_clear_bit(BC_FLG_BUSY, &X->Flag);
|
-     clear_bit(BC_FLG_BUSY, &X->Flag);
)
-   }
- }
+ bc_close(X);