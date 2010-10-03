@@
type T;
identifier f;
T *x;
@@

* T *f(...)
{
  ...
  x = kmalloc(...);
  ...
  return x;
}
