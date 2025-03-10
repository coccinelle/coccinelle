@@
struct sk_buff *skb;
@@

// and not 
// @@
// expression skb;
// @@


(
- &(skb->pkt_type)
+ &bt_cb(skb)->pkt_type
|
- skb->pkt_type
+ bt_cb(skb)->pkt_type
)
