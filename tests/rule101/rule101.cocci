// would like to say the following...
//@@
//struct sk_buff *skb;
//@@

@@
expression skb;
@@

(
- &(skb->pkt_type)
+ &bt_cb(skb)->pkt_type
|
- skb->pkt_type
+ bt_cb(skb)->pkt_type
)
