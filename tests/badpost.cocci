@x@
position p;
identifier f;
expression E;
@@

- f(3,
+ f(3,
   E@p,...)

@@
position x.p;
identifier g;
expression E;
@@

- g(3,E@p,...);