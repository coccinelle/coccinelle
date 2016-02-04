static int stir_transmit_thread(void *arg)
{

	while (x)
	{
		/* if suspending, then power off and wait */
		if (unlikely(freezing(current))) {
			refrigerator();

		}
	}
}

