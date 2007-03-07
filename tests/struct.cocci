@@
identifier X, Y;
@@

struct foo {
  ...
  struct bar X;
  ...
  struct xxx Y;
  ...
};

@@
struct foo *a;
@@

- f(a->X);

@@
struct foo *a;
@@

- f(a->Y);
