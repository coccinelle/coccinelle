//@@
//expression x;
//type T1, T2;
//expression e1, e2;
//@@
//(
//  x = (T1) kmalloc(...)
//|
//  x = kmalloc(...)
//)
//  ... when != x = e1
//  if(x == NULL) {
//    ... when != x = e2
//(
//    *x
//|
//    *((T2)x)
//|
//    f(...,x,...)
//)
//    ...
//  }

@@
expression x;
type T1, T2;
expression e;
expression f;
@@
(
  x = (T1) kmalloc(...)
|
  x = kmalloc(...)
)
  ... when != \( if(x == NULL) { ... return; } \| x = e; \)
//(
//  *x
//|
//  *((T2)x)
//|
  f(...,x,...)
//)
