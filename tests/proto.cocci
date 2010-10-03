@r@
identifier fn2;
identifier bcs, ev;
@@

- fn2(struct BCState *bcs, int ev) {
-   ...
-   bcs->event |= 1 << ev;
-   schedule_work(&bcs->work);
- }
