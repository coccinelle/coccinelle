// shows the expressive power added by positions
// normally, we find one root and then explore its children.
// here the root is in some sense the if, and we want to see if it can
// be reached from two contexts

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
