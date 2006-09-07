@@
//local function fn1;
identifier fn1;
identifier cs, ev;
@@

// supposed to drop the prototype as well

- fn1(struct IsdnCardState *cs, int ev) {
-   ...
(
-   test_and_set_bit(ev, &cs->event);
|
-   set_bit(ev, &cs->event);
|
-   cs->event |= 1 << ev; // found in config.c
)
-   schedule_work(&cs->work);
- }

@@
//struct IsdnCardState *cs;
//int ev;
expression cs, ev;
@@

- fn1(cs,ev)
+ sched_d_event(cs,ev)

@@
//struct IsdnCardState *cs;
//int ev;
expression ev;
identifier fn;
identifier cs;
@@

  fn(...,struct IsdnCardState *cs,...) {
  <...
(
-   test_and_set_bit(ev, &cs->event);
|
-   set_bit(ev, &cs->event);
|
-   cs->event |= 1 << ev;
)
-   schedule_work(&cs->work);
+   sched_d_event(cs,ev);
  ...>
  }

@@
//struct IsdnCardState *cs;
//int ev;
expression ev;
identifier cs;
@@

  struct IsdnCardState *cs;
  <...
(
- test_and_set_bit(ev, &cs->event);
|
- set_bit(ev, &cs->event);
|
- cs->event |= 1 << ev;
)
- schedule_work(&cs->work);
+ sched_d_event(cs,ev);
  ...>


