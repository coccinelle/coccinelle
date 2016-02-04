@@
!type A;
type A1;
identifier X, X1;
expression Y, Z;
@@

(
  A X;
  ooo
  A1 X1;
)
  <...
  request_irq(X->irq, Y, Z)
  ...
    <...
    X1->irq = X->irq;
    ...>
  ...>

@@
identifier X2;
local function fn;
@@

(
  fn(..., A1 X2, ...) {
|
  fn(...) {
    A1 X2;
)
    <...
-   synchronize_irq()
+   synchronize_irq(X2)
    ...>
  }
