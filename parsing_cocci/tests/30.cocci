@@
expression E;
@@

(
- if ((E->flags & (1 << TTY_DO_WRITE_WAKEUP)) && E->ldisc.write_wakeup)
|
- if (test_bit(TTY_DO_WRITE_WAKEUP, &E->flags) && E->ldisc.write_wakeup)
)
-    (E->ldisc.write_wakeup)(E);
+ tty_wakeup(E);
  ooo
- wake_up_interruptible(&E->write_wait);

@@
expression E;
@@

- if (E->ldisc.flush_buffer) E->ldisc.flush_buffer(E);
+ tty_ldisc_flush(E);
