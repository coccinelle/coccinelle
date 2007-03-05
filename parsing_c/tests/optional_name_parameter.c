typedef int request_arg_t;


static void do_ftl_request(int)
{
}

static void do_ftl_request(request_arg_t)
{
}




static int hub_thread(void *__unused)
{
	/*
	 * This thread doesn't need any user-level access,
	 * so get rid of all our resources
	 */

	daemonize("khubd");
	allow_signal(SIGKILL);

	/* Send me a signal to get me die (for debugging) */
	do {
		hub_events();
		wait_event_interruptible(khubd_wait, !list_empty(&hub_event_list)); 
		try_to_freeze(PF_FREEZE);
	} while (!signal_pending(current));

	pr_debug ("%s: khubd exiting\n", usbcore_name);
	complete_and_exit(&khubd_exited, 0);
}
