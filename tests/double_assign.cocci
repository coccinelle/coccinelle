@s@
identifier i;
position p0;
@@

i =@p0 ...;

@r exists@
identifier s.i;
position s.p0,p;
@@

i =@p0 ...;
...
i =@p <+... i ...+>;

@x@
identifier s.i;
position s.p0;
position p != r.p;
@@

- i =@p0 ...;
... when strict
  i =@p ...;
