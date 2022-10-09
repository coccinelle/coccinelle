static int torture_ww_mutex_lock(int tid)
__acquires(torture_ww_mutex_2)
{
	locks[2].lock = &torture_ww_mutex_2;
}
