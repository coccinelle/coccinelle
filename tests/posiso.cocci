@x@
position p1,p2;
expression E;
statement S1, S2;
@@

if@p1 (E ==@p2 NULL) S1 else S2

@@
position x.p1;
statement S1, S2;
@@

- if@p1 (...) S1 else S2
