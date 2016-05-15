#include "proto3.h"

static void bch_sched_event(struct BCState *bcs, int event)
{
	bcs->event |= 1 << event;
	schedule_work(&bcs->work);
}
