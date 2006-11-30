@@
//local function fn2;
identifier fn2;
identifier bcs, ev;
@@

// supposed to drop the prototype as well

- fn2(struct BCState *bcs, int ev) {
-   ...
(
-   test_and_set_bit(ev, &bcs->event);
|
-   set_bit(ev, &bcs->event);
|
-   bcs->event |= 1 << ev; // the only case that is used
)
-   schedule_work(&bcs->work);
- }

@@
struct BCState *bcs;
int ev;
@@

- fn2(bcs,ev)
+ sched_b_event(bcs,ev)

@@
struct BCState *bcs;
int ev;
@@

(
-   test_and_set_bit(ev, &bcs->event);
|
-   set_bit(ev, &bcs->event);
|
-   bcs->event |= 1 << ev; // the only case that is used
)
-   schedule_work(&bcs->work);
+   sched_b_event(bcs,ev);


@@
struct BCState *bcs;
int ev;
@@
// added due to experience - interfile effect
- hscx_sched_event(bcs,ev)
+ sched_b_event(bcs,ev)