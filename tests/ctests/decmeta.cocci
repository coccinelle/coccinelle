@@
idexpression decimal(TEN,FIVE) x;
@@

- x
+ 10

@@
idexpression decimal(10,5) x;
@@

- x
+ 20

@r@
constant n,p;
idexpression decimal(n,p) x;
@@

- x
+ p

@@
constant r.n;
@@

-6
+n

@@
@@

- decimal(10,5)
+ int

@@
@@

- decimal(TEN,FIVE)
+ int *
