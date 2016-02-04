// dumb example. does not work well because S is also match
// on the compounds, so it try to erase multiple times the same node.

@@
statement S;
@@

- S
+ S f(1);
