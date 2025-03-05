int main() {
  spin_lock(&isp116x->lock);
  /* take idle endpoints out of the schedule */
  if (!list_empty(&ep->hep->urb_list)) {
    spin_unlock(&isp116x->lock);
    return;
  }

  /* async deschedule */
  if (!list_empty(&ep->schedule)) {
    spin_unlock(&isp116x->lock);
    return;
  }
}
