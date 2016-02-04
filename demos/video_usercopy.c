int p20_ioctl(int cmd, void*arg) {
  switch(cmd) {
  case VIDIOGCTUNER: {
    struct video_tuner v;
    if(copy_from_user(&v,arg)!=0)
      return -EFAULT;
    if(v.tuner)
      return -EINVAL;
    v.rangelow = 87*16000;
    v.rangehigh = 108 * 16000;
    if(copy_to_user(arg,&v))
      return -EFAULT;
    return 0;
  }
  case AGCTUNER: {
    struct video_tuner v;     
    if(copy_from_user(&v,arg))
      { 
        return -EFAULT;
      }
    if(v.tuner)
      return -EINVAL;
    v.rangelow = 0;
    v.rangehigh = 0;
    if(copy_to_user(arg,&v))
      return -EFAULT;
    return 0;
  }
  }
}
