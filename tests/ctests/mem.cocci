@rule1@
identifier f;
@@

f(...) {
<+...
   dev_kfree_skb_irq(...)
...+>
}

@@
identifier rule1.f, fld;
identifier I;
type T;
@@

T I = {
*  .fld = f,
};
