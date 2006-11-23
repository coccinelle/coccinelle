int wd7000_info(int x) {
  float z;           
  scsi *y;
  z = x + 1;
  y = scsi_get();
  if(!y) { 
    kprintf("error");
    return -1; 
  }
  kprintf("val = %d", y->field + z);
  scsi_put(y);
  return 0;
}

