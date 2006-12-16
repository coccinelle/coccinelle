int main() {
  fsm->jumpmatrix =
    kmalloc(sizeof(FSMFNPTR) * fsm->state_count * fsm->event_count,
	    GFP_KERNEL);
  memset(fsm->jumpmatrix, 0,
	 sizeof(FSMFNPTR) * fsm->state_count * fsm->event_count);
}

