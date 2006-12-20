@@
expression x;
type T1, T2, T3;
expression e1, e2;
expression f;
identifier fld;
@@
  x = (T1) kmalloc(...)
  ... when != x = e1
  if(x == NULL) {
    ... when != x = e2
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
    ...
  }

@@
expression x;
type T1, T2, T3;
expression e1, e2;
expression f;
identifier fld;
statement S;
@@
  x = (T1) kmalloc(...)
  ... when != x = e1
  if(x != NULL) S else {
    ... when != x = e2
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
    ...
  }

// for some "good" reason, isomorphisms don't apply to whencode
@@
expression x;
type T1, T2, T3;
expression e;
expression f;
identifier fld;
statement S;
@@
  x = (T1) kmalloc(...);
  ... when != \( if(\(x == NULL\|NULL == x\|!x\)) { ... \(return;\|return e;\)   } \| if(\(x == NULL\|NULL == x\|!x\)) \(return;\|return e;\) \| if(\(x != NULL\|NULL != x\|x\)) S else { ... \(return;\|return e;\) } \| if(\(x != NULL\|NULL != x\|x\)) S else \(return;\|return e;\) \| x = e; \)
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

@@
expression x;
type T1;
expression e1, e2;
@@
  x = (T1) kmalloc(...)
  ... when != x = e1
  if(x == NULL) {
    ... when != x = e2
    return x;
  }

// for some "good" reason, isomorphisms don't apply to whencode
@@
expression x;
type T1;
expression e;
@@
  x = (T1) kmalloc(...);
  ... when != \( if(\(x == NULL\|NULL == x\|!x\)) { ... \(return;\|return e;\) } \| if(\(x == NULL\|NULL == x\|!x\)) \(return;\|return e;\) \| x = e; \)
  return x;
