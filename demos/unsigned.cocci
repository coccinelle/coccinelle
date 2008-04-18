// This semantic patch does essentially the same as the script in
// unsigned.txt.  That one does things with typedefs.  Not clear whether the
// semantic patch finds those cases (it didn't but maybe there aren't any).

@@
unsigned int i;
@@

* i < 0
