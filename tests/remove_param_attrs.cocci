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
