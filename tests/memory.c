#define SET_MAGIC(q,m)	((q)->magic = (m))
#define BAD_MAGIC(q,m)	((q)->magic != (m))
int __queue_add(Queue_t *queue, Scsi_Cmnd *SCpnt, int head)
{
	if (BAD_MAGIC(q, QUEUE_MAGIC_FREE))
		BUG();
}
