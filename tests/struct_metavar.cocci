@@
type T;
identifier X, Y;
@@

T {
  ...
  struct bar X;
  ...
  struct xxx Y;
  ...
};

@@
T *a;
@@

- f(a->X);

@@
T *a;
@@

- f(a->Y);
