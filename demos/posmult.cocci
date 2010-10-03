@n@
position p;
expression E;
statement S,S1;
@@

E = NULL
... when != E = ALLOC(...)
if@p (\(E\|!E\)) S else S1

@@
expression E, E1;
statement S,S1;
position p1 != n.p;
@@

* E = ALLOC(...)
... when != E = E1
* if@p1 (\(E\|!E\))
 S else S1
