@loc@
identifier f;
position pl;
@@

  f@pl(...)

@probe forall@
identifier E;
identifier f,g;
int ret;
statement S;
position loc.pl;
@@

  E = f@pl(...);
  ... when any
(
  g(...,E,...);
|
  return ret;
)

@exists@
identifier probe.f, probe.g;
@@

  f(...)
  ...
  g(...)