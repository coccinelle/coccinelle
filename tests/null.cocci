@@
expression x;
type T1, T2, T3;
expression e1, e2;
expression f;
identifier fld;
@@
(
  x = (T1) kmalloc(...)
|
  x = kmalloc(...)
)
  ... when != x = e1
  if(x == NULL) {
    ... when != x = e2
(
    *x
|
    *((T2)x)
|
    x->fld // want this to apply even as an LValue, because no iso for *x
|
    (x)->fld // want this to apply even as an LValue, because no iso for *x
|
    f(...,x,...)
|
    f(...,(T3)x,...)
)
    ...
  }

// for some "good" reason, isomorphisms don't apply to whencode
@@
expression x;
type T1, T2, T3;
expression e;
expression f;
identifier fld;
@@
(
  x = (T1) kmalloc(...)
|
  x = kmalloc(...)
)
  ... when != \( if(\(x == NULL\|NULL == x\|!x\)) { ... \(return;\|return e;\) } \| if(\(x == NULL\|NULL == x\|!x\)) \(return;\|return e;\) \| x = e; \)
(
  *x
|
  *((T2)x)
|
  x->fld
|
  (x)->fld
|
  f(...,x,...)
|
  f(...,(T3)x,...)
)
