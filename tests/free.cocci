@@
identifier x;
expression E;
type T;
@@

(
free(x);
|
kfree(x);
|
kfree_skb(x);
|
dev_kfree_skb_any(x);
)
... WHEN != \(T x = E; \| x = E;\)
x
