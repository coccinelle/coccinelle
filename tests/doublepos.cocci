// this shows a constraint on an inherited position variable

@a@
position p;
@@

f@p(1,...)

@b@
position p1;
@@

f@p1(...,5)

@@
position a.p != b.p1;
@@

- f@p(...);
