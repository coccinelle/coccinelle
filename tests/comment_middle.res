struct i {
        spinlock_t queue_lock;                                          /* spinlock for protecting mods on inqueue and outqueue */
        struct list_head inqueue, outqueue;                             /* queued frame list and ready to dequeue frame list */
        wait_queue_head_t wait_frame;                                   /* Processes waiting */
};

int main() {
}
