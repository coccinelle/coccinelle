typedef struct _drm_via_blitq {
	struct work_struct wq;
} drm_via_blitq_t;

void
via_init_dmablit(drm_device_t *dev)
{
	drm_via_blitq_t *blitq;

	INIT_WORK(&blitq->wq, via_dmablit_workqueue,blitq);
}
