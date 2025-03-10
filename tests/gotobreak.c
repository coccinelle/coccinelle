static void sedlbauer_config(struct pcmcia_device *link)
{
    while (1) {
	if ((cfg->mem.nwin > 0) || (dflt.mem.nwin > 0)) {
	  goto next_entry;
	}
	/* If we got this far, we're cool! */
	break;

    next_entry:
	CS_CHECK(GetNextTuple, pcmcia_get_next_tuple(link, &tuple));
    }

    return;
}
