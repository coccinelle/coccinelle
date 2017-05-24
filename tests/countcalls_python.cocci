@initialize:python@
@@

coccinelle.tbl = {}
local = set()

def inc(f):
  try:
    coccinelle.tbl[f] += 1
  except KeyError:
    coccinelle.tbl[f] = 1

@script:python@
@@
local.clear()

@r@
identifier f;
@@

f(...) { ... }

@script:python@
f << r.f;
@@

local.add(f)

@r1@
identifier f;
type T;
@@

T f(...);

@script:python@
f << r1.f;
@@

local.add(f)

@s@
identifier f;
@@
f(...);

@script:python@
f << s.f;
@@

if f not in local and f.lower() == f:
  inc(f)

@finalize:python@
tbls << merge.tbl;
@@
tbl = tbls[0]
for i in range(1, len(tbls)):
  tbl.update(tbls[i])
for (v, f) in sorted([(v, k) for (k, v) in tbl.items()], reverse = True):
  print("{}: {}".format(f, v))
