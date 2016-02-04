@r@
expression E;
statement S;
@@

f(3 +@E@S 4);

@@
expression r.E;
@@

- g(E)
+ h(E)

@@
expression x;
statement r.S;
@@

- if
+ while
  (x) S

@s@
expression E;
position p;
@@

-m(3 +@E@p 3);

@@
expression s.E;
@@

- g(E)
+ q(E)

@a@
declaration d;
@@

-int x@d;

@@
declaration a.d;
@@

int y;
+d
