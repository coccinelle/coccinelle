static void mppe_free(void *arg)
{
	struct ppp_mppe_state *state = (struct ppp_mppe_state *) arg;
	if (state) {
	    if (state->sha1_digest)
		kfree(state->sha1_digest);
	    kfree(state);
	}
}
