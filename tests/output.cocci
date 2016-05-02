@r@
identifier f;
@@

f(...) { ... }

@script:python depends on r@
f << r.f;
@@

print(f)