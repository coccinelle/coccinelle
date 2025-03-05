@ r1 @
identifier X,Y;
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
identifier r1.X;
@@

- f(a->X);

@@
struct foo *a;
identifier r1.Y;
@@

- f(a->Y);
