@@
expression y;
expression E;
type T;
@@

(
free(y);
|
kfree(y);
|
kfree_skb(y);
|
dev_kfree_skb(y);
|
dev_kfree_skb_any(y);
)
... WHEN != y = E;
y
