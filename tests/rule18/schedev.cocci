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
expression ev;
@@

- fn2(bcs,ev)
+ sched_b_event(bcs,ev)

@@
struct BCState *bcs;
expression ev;
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
// in one case where this rule is used, the first argument is a complex
// expression, so we can't figure out the type, but the type should be
// fixed by the called function, so perhaps there is no need for the rule to
// check for it
//struct BCState *bcs;
expression bcs;
expression ev;
@@
// added due to experience - interfile effect
- hscx_sched_event(bcs,ev)
+ sched_b_event(bcs,ev)
