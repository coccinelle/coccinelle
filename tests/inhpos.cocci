// the point of this example is that if p doesn't get bound, then p1 can
// be anything

@a@
position p;
@@

f@p(...)

@@
position p1 != a.p;
@@

- g@p1(...);
