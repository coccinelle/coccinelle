@initialize:python@
@@

coccinelle.tbl = {}
local = set()

def inc(f):
  try:
    coccinelle.tbl[f] += 1
  except KeyError:
    coccinelle.tbl[f] = 1

def merge_dicts(dicts, op):
  result = {}
  for dict in dicts:
    for (k, v) in dict.items():
      try: result[k] = op(result[k], v)
      except KeyError: result[k] = v
  return result

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
tbl = merge_dicts(tbls, lambda (a, b): a + b).items()
for (v, f) in sorted([(v, k) for (k, v) in tbl], reverse = True):
  print("{}: {}".format(f, v))
