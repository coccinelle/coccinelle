// The first two rules only work when there is only one reference.

@@
expression *x;
type T;
expression e;
expression f;
@@
  if(x == NULL) {
    ... when != x = e
(
     return x;
|
(
     *x
|
     *((T)x)
|
     f(...,x,...)
|
     f(...,(T)x,...)
)
)
    ...
  }

@@
expression *x;
type T;
expression e;
expression f;
statement S;
@@
  if(x != NULL) S else {
    ... when != x = e
(
    return x;
|
(
    *x
|
    *((T)x)
|
    f(...,x,...)
|
    f(...,(T)x,...)
)
)
    ...
  }

@@
expression *x;
type T1, T2, T3;
expression e;
expression f;
statement S;
@@
  x = (T1) kmalloc(...);
  ... when != \( if(x == NULL) { ... return ...; } \| if(x == NULL) S else { ... return ...; } \| x = e; \)
(
    return x;
|
(
    *x
|
    *((T2)x)
|
    f(...,x,...)
|
    f(...,(T3)x,...)
)
)
