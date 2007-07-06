typedef struct {
	struct semaphore    lock;
} scsi_changer;


int main (scsi_changer *x) {
  foo(x->new_lock);
}

struct  scsi_changer_two {
  struct semaphore    lock;
};


int main (struct scsi_changer_two *x) {
  foo(x->new_lock);
}
