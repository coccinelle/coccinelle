@@
?local function fn1;
identifier cs, event;
@@

(
- fn1(struct IsdnCardState *cs, int event) {
-   ...
-   test_and_set_bit(event, &cs->event);
-   schedule_work(&cs->work);
- }
|
- fn1(struct IsdnCardState *cs, int event);
)

@@
struct IsdnCardState *cs;
int event;
@@

- fn1(cs,event)
+ sched_d_work(cs,event)

@@
struct IsdnCardState *cs;
int event;
@@

- test_and_set_bit(event, &cs->event);
- schedule_work(&cs->work);
+ sched_d_work(cs,event);

@@
?local function fn2;
identifier bcs, event;
@@

(
- fn2(struct BCState *bcs, int event) {
-   ...
-   bcs->event |= 1 << event;
-   schedule_work(&bcs->work);
- }
|
- fn2(struct BCState *bcs, int event);
)

@@
struct BCState *bcs;
int event;
@@

- fn2(bcs,event)
+ sched_b_work(bcs,event)

@@
struct BCState *bcs;
int event;
@@

- bcs->event |= 1 << event;
- schedule_work(&bcs->work);
+ sched_b_work(bcs,event)
