static struct block_device_operations gscd_fops = {

  .ep[0] = {}
};



char *ioctl_names[CM_IOC_MAXNR + 1] = {
  [_IOC_NR(CM_IOCGSTATUS)] "CM_IOCGSTATUS",
  [_IOC_NR(CM_IOCGATR)] "CM_IOCGATR",
  [_IOC_NR(CM_IOCARDOFF)] "CM_IOCARDOFF",
  [_IOC_NR(CM_IOCSPTS)] "CM_IOCSPTS",
  [_IOC_NR(CM_IOSDBGLVL)] "CM4000_DBGLVL",
  [EXT2_FT_UNKNOWN]	DT_UNKNOWN,
};


