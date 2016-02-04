@@
struct IsdnCard v;
local function fn;
@@

v.irq_func = &fn;

@@
identifier intno, dev_id, regs;
@@

   fn(int intno, void *dev_id, struct pt_regs *regs) {
     <...
(
-    spin_lock(...);
|
-    spin_unlock(...);
)
     ...>
   }

@@
identifier cs;
@@

   fn(int intno, void *dev_id, struct pt_regs *regs) {
     ...
     struct IsdnCardState *cs = dev_id;
+    spin_lock(&cs->lock);
     ...
?-   if (!cs) { ... }
     ...
+    spin_unlock(&cs->lock);
     return;
   }
