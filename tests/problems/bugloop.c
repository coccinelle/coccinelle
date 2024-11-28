/* this doesn't work, because on the paths where we don't find refrigerator,
we expect to reach Exit without first going through current->flags & PF_FREEZE,
but of course any path that goes around the loop does precisely that */

static int stir_transmit_thread(void *arg)
{

	while (x)
	{
		/* if suspending, then power off and wait */
		if (unlikely(current->flags & PF_FREEZE)) {
			refrigerator(PF_FREEZE);

		}
	}
}

