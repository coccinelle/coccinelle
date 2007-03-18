@@
struct IsdnCardState cs;
//local function interrupt;
identifier interrupt;
@@

cs.irq_func = &interrupt;

@@
identifier intno, dev_id, regs, cs;
statement S;
identifier flags;
@@

interrupt(int intno, void *dev_id, struct pt_regs *regs) {
  ...
  struct IsdnCardState *cs = dev_id;
  ...
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
|
- save_flags(flags); cli();
|
- restore_flags(flags);
)
  ...>
+   spin_unlock(&cs->lock);
    return;
}

@@
@@

interrupt(int intno, void *dev_id, struct pt_regs *regs) {
  ...
- long flags;
  ... when != flags
}

// --------------------------------------------------------------------
// seems like rules for another kind of interrupt function

@@
struct IsdnCardState *cs;
@@

- if ((cs->tx_skb = skb_dequeue(&cs->sq))) { ... }
- else sched_d_event(cs, D_XMTBUFREADY);
+ xmit_ready_d(cs);

@@
identifier d_fill_fifo, cs;
@@

d_fill_fifo(struct IsdnCardState *cs)
{
	...
	xmit_fill_fifo_d(...)
	...
}

@@
struct IsdnCardState *cs;
identifier E;
@@

(
	cs->BC_Send_Data = E;
+	cs->DC_Send_Data = d_fill_fifo;
|
	cs->BC_Send_Data = &E;
+	cs->DC_Send_Data = &d_fill_fifo;
)

@@
struct IsdnCardState *cs;
identifier E;
@@

 	... when != cs->DC_Send_Data
(
+	cs->DC_Send_Data = d_fill_fifo;
	cs->DC_Close = E;
|
+	cs->DC_Send_Data = &d_fill_fifo;
	cs->DC_Close = &E;
)
 	... when != cs->DC_Send_Data

@@
identifier b_fill_fifo, cs;
@@

b_fill_fifo(struct IsdnCardState *cs)
{
	...
	xmit_fill_fifo_b(...)
	...
}

@@
struct IsdnCardState *cs;
identifier E;
@@

 	... when != cs->BC_Send_Data
(
+	cs->BC_Send_Data = b_fill_fifo;
	cs->BC_Close = E;
|
+	cs->BC_Send_Data = &b_fill_fifo;
	cs->BC_Close = &E;
)
 	... when != cs->BC_Send_Data
