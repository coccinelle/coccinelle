@r@
identifier f,s;
position p;
@@

enum f { ...,
  s,@p
  ..., };

@script:python@
p << r.p;
@@

print p