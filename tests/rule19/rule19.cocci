@@
//local function interrupt;
identifier interrupt;
identifier intno, dev_id, regs, cs;
statement S;
@@

interrupt(int intno, void *dev_id, struct pt_regs *regs) {
  ...
  struct IsdnCardState *cs = dev_id;
- if (!cs) { ... return; }
+ spin_lock(&cs->lock);
  <...
(
- spin_lock(...);
|
- spin_unlock(...);
|
- spin_lock_irqsave(...);
|
- spin_unlock_irqrestore(...);
)
  ...>
}

// awkward to have to split, but the statement originally before might be
// something we want to remove in the nest
@@
statement S;
@@

  interrupt(...) {
    ...
(
+   spin_unlock(&cs->lock);
    return;
|
    S
+   spin_unlock(&cs->lock);
)
  }
