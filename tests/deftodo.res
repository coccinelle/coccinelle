#define UNIT_TYPE int

/*
 * cpu_alloc area immediately follows the percpu area that is allocated for
 * each processor.
 */
#define cpu_alloc_start ((int *)__per_cpu_end)

void __init cpu_alloc_init(void)
{
	cpu_alloc_start = alloc_bootmem(nr_units * UNIT_SIZE);
}

