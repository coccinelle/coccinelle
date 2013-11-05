int main() {
        spin_lock(&isp116x->lock);
	a();
	b();
        /* take idle endpoints out of the schedule */
        if (!list_empty(&ep->hep->urb_list)) {
          returnn();
	}

        /* async deschedule */
         if (!list_empty(&ep->schedule)) {
	   returnn();
         }
}
