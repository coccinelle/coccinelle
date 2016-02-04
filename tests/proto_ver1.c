void bch_l2l1(struct PStack *st, int pr, void *arg);
void bch_sched_event(struct BCState *bcs, int event);
void bch_empty_fifo(struct BCState *bcs, int count);

void
bch_sched_event(struct BCState *bcs, int event)
{
	bcs->event |= 1 << event;
	schedule_work(&bcs->work);
}
