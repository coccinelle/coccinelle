static void redo_fd_request(void)
{
	unsigned long flags;

 repeat:
	if (blk_queue_empty(QUEUE)) {
		/* Nothing left to do */
		return;
	}


	end_request(1);
	goto repeat;
}
