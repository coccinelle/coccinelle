@@
identifier v;
identifier fld;
expression E;
@@

  struct foo v;
  m();
  <...
  f(E);
  <...
* v.fld
  ...>
  g(E);
  ...>
  n();
