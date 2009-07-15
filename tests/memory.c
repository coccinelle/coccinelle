#define QUEUE_MAGIC_FREE	0xf7e1c9a3
#define BAD_MAGIC(q,m)	((q)->magic != (m))


int __queue_add(Queue_t *queue, Scsi_Cmnd *SCpnt, int head)
{
	QE_t *q;
	if (BAD_MAGIC(q, QUEUE_MAGIC_FREE))
		BUG();
}
