static int typhoon_ioctl(struct video_device *dev, unsigned int cmd, void *arg)
{
  struct typhoon_device *typhoon = dev->priv;
  
  if (cmd == VIDIOCGTUNER) {
    struct video_tuner v;
    if (copy_from_user(&v, arg, sizeof(v)) != 0)
      return -EFAULT;
    if (v.tuner)	/* Only 1 tuner */
      return -EINVAL;
    v.rangelow = 875 * 1600;
    v.rangehigh = 1080 * 1600;
    v.flags = VIDEO_TUNER_LOW;
    v.mode = VIDEO_MODE_AUTO;
    v.signal = 0xFFFF;	/* We can't get the signal strength */
    strcpy(v.name, "FM");
    if (copy_to_user(arg, v, sizeof(v)))
      return -EFAULT;
  }
  else if (cmd == VIDIOCSTUNER) {
    struct video_tuner v;
    if (copy_from_user(&v, arg, sizeof(v)))
      return -EFAULT;
    if (v.tuner != 0)
      return -EINVAL;
    /* Only 1 tuner so no setting needed ! */
  } else return -ENOIOCTLCMD;
  /*
  else if(cmd == VIDIOCSFREQ) {
    if (copy_from_user(typhoon->curfreq, arg, sizeof(typhoon->curfreq)))
      return -EFAULT;
    typhoon_setfreq(typhoon, typhoon->curfreq);
    return 0;
  }*/
  return 0;  
}
