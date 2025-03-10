static void bch_l2l1(struct PStack *st, int pr, void *arg);
static void bch_sched_event(int event);
static void bch_empty_fifo(struct BCState *bcs, int count);

static void
bch_sched_event(int event) {
	bcs->event |= 1 << event;
	schedule_work(&bcs->work);
}
