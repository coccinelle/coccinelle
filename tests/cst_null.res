void* videobuf_alloc(unsigned int size)
{
        struct videobuf_buffer *vb;

        vb = kzalloc(size, GFP_KERNEL);
        if (vb != NULL) {
                videobuf_dma_init(&vb->dma);
                init_waitqueue_head(&vb->done);
                vb->magic     = MAGIC_BUFFER;
        }
        return vb;
}


void* videobuf_alloc(unsigned int size)
{
        struct videobuf_buffer *vb;

        vb = kzalloc(size, GFP_KERNEL);
        if (vb) {
                videobuf_dma_init(&vb->dma);
                init_waitqueue_head(&vb->done);
                vb->magic     = MAGIC_BUFFER;
        }
        return vb;
}


void* videobuf_alloc(unsigned int size)
{
 	struct videobuf_buffer *vb;

	vb = kzalloc(size, GFP_KERNEL);
	if (NULL != vb) {
		videobuf_dma_init(&vb->dma);
		init_waitqueue_head(&vb->done);
		vb->magic     = MAGIC_BUFFER;
	}
        return vb;
}
