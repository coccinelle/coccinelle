void main (int i) 
{

#if 0
	/* May not be able to do this - chip my not have been set up yet */
	tmp = hostdata->this_id_mask = NCR53c7x0_read8(SCID_REG);
	for (host->this_id = 0; tmp != 1; tmp >>=1, ++host->this_id);
#else
	host->this_id = 7;
#endif

#if 0
	    event = hostdata->events[i];
#else
	    memcpy ((void *) &event, (void *) &(hostdata->events[i]),
		sizeof(event));
#endif



}
