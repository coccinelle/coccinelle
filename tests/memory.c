#define BAD_MAGIC(q,m) 0

int __queue_add(Queue_t *queue, Scsi_Cmnd *SCpnt, int head)
{
	if (BAD_MAGIC(1,12))
		BUG();
}
