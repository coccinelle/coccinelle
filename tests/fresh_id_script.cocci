@initialize:ocaml@
@@

let ctr = ref 0

let make_fresh s =
  ctr := !ctr + 1;
  Printf.sprintf "fresh%s%d" s !ctr

@initialize:python@
@@

ctr = 0

def make_fresh(s):
  global ctr
  ctr = ctr + 1;
  return "fresh" + s + str(ctr)

@r@
int j;
@@
  foo(j);

@@
int i;
fresh identifier fresh_py0 = script:python() { make_fresh("") };
fresh identifier fresh_py1 = script:python(i) { make_fresh(i) };
fresh identifier fresh_py2 = script:python(i, r.j) { make_fresh(j) };
fresh identifier fresh_ml0 = script:ocaml() { make_fresh("") };
fresh identifier fresh_ml1 = script:ocaml(i) { make_fresh(i) };
fresh identifier fresh_ml2 = script:ocaml(i, r.j) { make_fresh(j) };
@@

- foo(i);
+ foo(fresh_py0 + fresh_py1 + fresh_py2 + fresh_ml0 + fresh_ml1 + fresh_ml2);
+ foo(fresh_py0 + fresh_py1 + fresh_py2 + fresh_ml0 + fresh_ml1 + fresh_ml2);
