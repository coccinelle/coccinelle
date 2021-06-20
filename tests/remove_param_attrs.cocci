@@
type T,U;
attribute name __nocast;
identifier x,y;
@@

T x(
    ...,
    U y
-	__nocast
    ,
    ...
  ) {...}

@@
type T,U;
identifier x,y;
@@

T x(
    ...,
    U
-	__attribute__((nocast))
    y,
    ...
  ) {...}
