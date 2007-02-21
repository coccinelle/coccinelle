#define MERGEFCT(_FUNCTION, _BACK_FRONT, _CLUSTER, _DMA)		\
static int _FUNCTION(request_queue_t * q,				\
		     struct request * req,				\
		     struct buffer_head * bh,				\
		     int max_segments)					\
{									\
    int ret;								\
    SANITY_CHECK(req, _CLUSTER, _DMA);					\
    ret =  __scsi_ ## _BACK_FRONT ## _merge_fn(q,			\
					       req,			\
					       bh,			\
					       max_segments,		\
					       _CLUSTER,		\
					       _DMA);			\
    return ret;								\
}

/* Version with use_clustering 0 and dma_host 1 is not necessary,
 * since the only use of dma_host above is protected by use_clustering.
 */
MERGEFCT(scsi_back_merge_fn_, back, 0, 0)
MERGEFCT(scsi_back_merge_fn_c, back, 1, 0)
MERGEFCT(scsi_back_merge_fn_dc, back, 1, 1)

MERGEFCT(scsi_front_merge_fn_, front, 0, 0)
MERGEFCT(scsi_front_merge_fn_c, front, 1, 0)
MERGEFCT(scsi_front_merge_fn_dc, front, 1, 1)

void main(int i) {
}
