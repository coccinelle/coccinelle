typedef struct {
	struct semaphore    lock;
} scsi_changer;


int main1 (scsi_changer *x) {
  foo(x->new_lock);
}

struct  scsi_changer_two {
  struct semaphore    lock;
};


int main2 (struct scsi_changer_two *x) {
  foo(x->new_lock);
}
