
void main(int i)
{
#if RAID5_DEBUG
	for (i = 0; i < MD_SB_DISKS; i++) {
#else
	for (i = 0; i < conf->working_disks+conf->failed_disks; i++) {
#endif

          foo();
        }
}
