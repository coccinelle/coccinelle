static void mppe_free(void *arg)
{
	struct ppp_mppe_state *state = (struct ppp_mppe_state *) arg;
	if (state) {
	    kfree(state->sha1_digest);
	    kfree(state);
	}
}
