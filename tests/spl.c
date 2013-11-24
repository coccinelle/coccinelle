int main() {
  spin_lock(&isp116x->lock);
  /* take idle endpoints out of the schedule */
  if (!list_empty(&ep->hep->urb_list)) {
    return;
  }

  /* async deschedule */
  if (!list_empty(&ep->schedule)) {
    return;
  }
}
