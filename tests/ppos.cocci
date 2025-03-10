@a@
position p;
identifier f;
expression E;
@@

f(...) {
 <... when strict
(
E@p = ERR_PTR(...)
|
E@p = NULL
)
...>
return E;
}

@b exists@
identifier f, fld;
expression E,E1,E2;
position p1, p2 != a.p;
@@

f(...) {
(
  ... when any
  E@p2 = E1
  ... when != E->fld
      when != E = E2
  return@p1 E;
|
  ... when != E->fld
      when != E = E2
  return@p1 E;
)
}
