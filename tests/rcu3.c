static int nvram_remove_os_partition(void)
{
  struct list_head *i;
  struct list_head *j;
  
  list_for_each(i, &nvram_part->partition) {
    part = list_entry(i, struct nvram_partition, partition);
    list_for_each_prev(j, &part->partition) {
      cur_part = list_entry(j, struct nvram_partition, partition);
    }
    
  }
  
  return 0;
}
