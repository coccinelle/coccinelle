void* videobuf_alloc(unsigned int size)
{
        struct videobuf_buffer *vb;

        vb = kmalloc(size,GFP_KERNEL);
        if (vb != NULL) {
                memset(vb,0,size);
                videobuf_dma_init(&vb->dma);
                init_waitqueue_head(&vb->done);
                vb->magic     = MAGIC_BUFFER;
        }
        return vb;
}


void* videobuf_alloc(unsigned int size)
{
        struct videobuf_buffer *vb;

        vb = kmalloc(size,GFP_KERNEL);
        if (vb) {
                memset(vb,0,size);
                videobuf_dma_init(&vb->dma);
                init_waitqueue_head(&vb->done);
                vb->magic     = MAGIC_BUFFER;
        }
        return vb;
}


void* videobuf_alloc(unsigned int size)
{
 	struct videobuf_buffer *vb;

	vb = kmalloc(size,GFP_KERNEL);
	if (NULL != vb) {
		memset(vb,0,size);
		videobuf_dma_init(&vb->dma);
		init_waitqueue_head(&vb->done);
		vb->magic     = MAGIC_BUFFER;
	}
        return vb;
}
