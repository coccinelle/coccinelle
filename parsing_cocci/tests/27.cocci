@@
local function f;
struct IsdnCard card;
@@

  card.irq_func = f

@@
fresh identifier mode_switch;
@@

  f(...
+   , int mode_switch
   ) {
    ...
  }
