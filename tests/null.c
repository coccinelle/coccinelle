int main() {
  fsm->jumpmatrix =
    kmalloc(sizeof(FSMFNPTR) * fsm->state_count * fsm->event_count,
	    GFP_KERNEL);
  if (fsm->jumpmatrix == NULL) { foo(fsm->jumpmatrix); return; }
  memset(fsm->jumpmatrix, 0,
	 sizeof(FSMFNPTR) * fsm->state_count * fsm->event_count);
}

