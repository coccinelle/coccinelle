inline int f(int i);

inline  float j;

inline int f2(int i) {
  return i+1;
}


void inline hdlc_sched_event(struct BCState *bcs, int event)
{
 	bcs->event |= 1 << event;
 	schedule_work(&bcs->work);
}
