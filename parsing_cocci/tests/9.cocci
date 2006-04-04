@@
expression E1, E2;
expression X, Y;
@@

(
- E1.l1.l1l2 = E2
+ E1.l2.l1l2 = E2
|
- E1.l2.l2l1 = E2
+ E1.l1.l2l1 = E2
|
- E1.l2.l2l3 = E2
+ E1.l3.l2l3 = E2
|
- E1.l3.l3l4 = E2
+ E1.lli.l3l4 = E2
|
- E1.l3.l3l2 = E2
+ E1.l2.l3l2 = E2
|
- E1.lli.l4l3 = E2
+ E1.l3.l4l3 = E2
|
- E1.lli.l4l3_proto = E2
+ E1.l3.l4l3_proto = E2
|
- E1->l1.l1l2(E1, X, Y)
+ L1L2(E1, X, Y)
|
- E1->l1.l2l1(E1, X, Y)
+ L2L1(E1, X, Y)
|
- E1->l1.l2l3(E1, X, Y)
+ L2L3(E1, X, Y)
|
- E1->l1.l3l2(E1, X, Y)
+ L3L2(E1, X, Y)
|
- E1->l1.l3l4(E1, X, Y)
+ L3L4(E1, X, Y)
|
- E1->lli.l4l3(E1, X, Y)
+ L4L3(E1, X, Y)
)
