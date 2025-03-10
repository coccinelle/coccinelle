@ rule1 @
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
rule1.T *a;
identifier rule1.X;
@@

- f(a->X);

@@
rule1.T *a;
identifier rule1.Y;
@@

- f(a->Y);
