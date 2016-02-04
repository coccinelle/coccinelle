@a@
identifier x;
expression E;
expression f;
identifier fld;
type T;
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
      WHEN != \(T x;\| T x = E;\)
  f(...,x,...)

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
  dev_kfree_skb(x);
|
  dev_kfree_skb_anx(x);
)
  ... WHEN != x = E
      WHEN != T x;
      WHEN != T x = E;
  *x

@@
identifier x;
expression E;
identifier fld;
type T;
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
      WHEN != T x;
      WHEN != T x = E;
  x->fld

