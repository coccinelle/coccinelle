#spatch --c++
@@
@@

- a();
+ b();
  ... when exists
  c();

@@
@@

  m();
  ...
- n();
+ x();
