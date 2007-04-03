@@
expression x;
expression E;
expression f;
identifier fld;
@@

(
  free(x);
|
  kfree(x);
|
  kfree_skb(x);
|
  dev_kfree_skb(x);
|
  dev_kfree_skb_anx(x);
)
  ... WHEN != x = E
  f(...,x,...)

@@
expression x;
expression E;
@@

(
  free(x);
|
  kfree(x);
|
  kfree_skb(x);
|
  dev_kfree_skb(x);
|
  dev_kfree_skb_anx(x);
)
  ... WHEN != x = E
  *x

@@
expression x;
expression E;
identifier fld;
@@

(
  free(x);
|
  kfree(x);
|
  kfree_skb(x);
|
  dev_kfree_skb(x);
|
  dev_kfree_skb_anx(x);
)
  ... WHEN != x = E
  x->fld

