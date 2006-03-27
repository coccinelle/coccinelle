@@
!local function f;
struct IsdnCard card;
@@

  card.irq_func = f

@@
@@

- f(...) {
+ f(..., int mode_switch) {
    ...
  }
