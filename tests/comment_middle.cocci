@r@
field f;
comments c;
identifier i;
@@

struct i { ...
  f@c
  ...
}

@script:ocaml@
f << r.f;
c << r.c;
@@

let (_,m,_) = List.hd c in
if not (m = []) then Coccilib.include_match false

@depends on r@
@@

- foo();
