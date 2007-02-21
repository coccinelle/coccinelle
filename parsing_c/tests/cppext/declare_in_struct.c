typedef struct {
    DECLARE_BITMAP(allocated, MAX_TAGS);
    int		nr_allocated;
    int		queue_size;
} TAG_ALLOC;
