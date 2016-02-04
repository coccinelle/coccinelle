@r@
identifier x;
@@

int x;

@script:python@
x << r.x;
@@

print x

@rr@
identifier x;
@@

struct x { ... int x; ... };

@script:python@
x << rr.x;
@@

print "name",x