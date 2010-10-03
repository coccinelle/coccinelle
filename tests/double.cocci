@@ expression E; @@

(
- (!skb_queue_len(E))
+ skb_queue_empty(E)
|
- (skb_queue_len(E) == 0)
+ skb_queue_empty(E)
)
