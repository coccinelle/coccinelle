void blk_queue_prep_rq(struct request_queue *q, prep_rq_fn *pfn)
{
 	q->prep_rq_fn = pfn;
}
