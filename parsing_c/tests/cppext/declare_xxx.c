void main(int i)
{
	DECLARE_COMPLETION(wait);
	int err = 0;
}

typedef struct {
  DECLARE_BITMAP(allocated, MAX_TAGS);

  DECLARE_BITMAP(unitbitmap, 32*32);
};


static DECLARE_WAIT_QUEUE_HEAD(slm_wait);	/* waiting for buffer */
static DECLARE_WAIT_QUEUE_HEAD(print_wait);	/* waiting for printing finished */

/* status codes */
#define	SLMSTAT_OK		0x00
#define	SLMSTAT_ORNERY	0x02


void main(int i)
{

	DECLARE_MUTEX_LOCKED(sem);
	scsi_qla_host_t *ha;
}
