@@
idexpression struct list_head *I;
@@

- I
+ _Y(I)

@@
identifier I;
expression E;
iterator list_for_each;
statement S;
@@

- list_for_each(_Y(I),E)
+ list_for_each(_X(I),E)
  S

@@
type T;
identifier I;
expression E;
@@

- list_entry(_Y(I),T,E)
+ list_entry(_X(I),T,E)

@ra@
type T;
identifier I, x;
expression E, E1, E2;
iterator list_for_each_entry;
@@

  <... when != _Y(I)
\+- list_for_each(_X(I),E1)
+   list_for_each_entry(x,E1,E2)
\+  {
      ... when != \(_Y(I)\|_X(I)\)
-     x = list_entry(_X(I),T,E2);
      ... when != \(_Y(I)\|_X(I)\|x=E\)
\+  }
  ...>

