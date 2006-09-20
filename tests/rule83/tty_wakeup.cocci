@@
expression E;
@@

- if (
(
-     (E->flags & (1 << TTY_DO_WRITE_WAKEUP))
|
-     test_bit(TTY_DO_WRITE_WAKEUP, &E->flags)
)
-  && \( E->ldisc.write_wakeup \| (E->ldisc.write_wakeup != NULL) \) ) {
-   ... when = \( printk(...); \| dbg(...); \)
-     	(E->ldisc.write_wakeup)(E); }
+ tty_wakeup(E);
  ...
- wake_up_interruptible(&E->write_wait);

@@
expression E;
@@

- if (
(
-     (E->flags & (1 << TTY_DO_WRITE_WAKEUP))
|
-     test_bit(TTY_DO_WRITE_WAKEUP, &E->flags)
)
-  && \( E->ldisc.write_wakeup \| (E->ldisc.write_wakeup != NULL) \) ) {
-   ... when = \( printk(...); \| dbg(...); \)
-     	E->ldisc.write_wakeup(E); }
+ tty_wakeup(E);
  ...
- wake_up_interruptible(&E->write_wait);

@@
expression E;
@@

- wake_up_interruptible(&E->write_wait);
  ...
- if (
(
-     (E->flags & (1 << TTY_DO_WRITE_WAKEUP))
|
-     test_bit(TTY_DO_WRITE_WAKEUP, &E->flags)
)
-  && \( E->ldisc.write_wakeup \| (E->ldisc.write_wakeup != NULL) \) ) {
-   ... when = \( printk(...); \| dbg(...); \)
-     	(E->ldisc.write_wakeup)(E); }
+ tty_wakeup(E);

@@
expression E;
@@

- wake_up_interruptible(&E->write_wait);
  ...
- if (
(
-     (E->flags & (1 << TTY_DO_WRITE_WAKEUP))
|
-     test_bit(TTY_DO_WRITE_WAKEUP, &E->flags)
)
-  && \( E->ldisc.write_wakeup \| (E->ldisc.write_wakeup != NULL) \) )
- { ... when = \( printk(...); \| dbg(...); \)
-     	E->ldisc.write_wakeup(E); }
+ tty_wakeup(E);

@@
expression E;
@@

- if (E->ldisc.flush_buffer != NULL) {
-   ... when = \( printk(...); \| dbg(...); \)
-   E->ldisc.flush_buffer(E); }
+ tty_ldisc_flush(E);

@@
expression E;
@@

- if (E->ldisc.flush_buffer != NULL) {
-   ... when = \( printk(...); \| dbg(...); \)
-   (E->ldisc.flush_buffer)(E); }
+ tty_ldisc_flush(E);
