@@
//local function interrupt;
identifier interrupt;
identifier intno, dev_id, regs, cs;
@@

interrupt(int intno, void *dev_id, struct pt_regs *regs) {
  ...
  struct IsdnCardState *cs = dev_id;
  ...
- if (!cs) { ... return; }
+ spin_lock(&cs->lock);
  <...
(
  spin_lock(...);
|
  spin_unlock(...);
|
  spin_lock_irqsave(...);
|
  spin_lock_irqrestore(...);
)
  ...>
+ spin_unlock(&cs->lock);
  return;
}

@@
//local function interrupt;
identifier interrupt;
identifier intno, dev_id, regs, cs;
@@

interrupt(int intno, void *dev_id, struct pt_regs *regs) {
  ...
  struct IsdnCardState *cs = dev_id;
  ...
- if (!cs) { ... return; }
+ spin_lock(&cs->lock);
  <...
(
  spin_lock(...);
|
  spin_unlock(...);
|
  spin_lock_irqsave(...);
|
  spin_lock_irqrestore(...);
)
  ...>
+ spin_unlock(&cs->lock);
}
