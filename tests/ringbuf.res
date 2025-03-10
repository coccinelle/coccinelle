struct bpf_ringbuf {
	spinlock_t spinlock ____cacheline_aligned_in_smp;
	atomic_t busy ____cacheline_aligned_in_smp;
};
