static int typhoon_ioctl(struct video_device *dev, unsigned int cmd, void *arg)
{
  struct typhoon_device *typhoon = dev->priv;
  
  if (cmd == VIDIOCGTUNER) {
    struct video_tuner v;
    if (copy_from_user(v, arg, sizeof(v)) != 0)
      ret(-EFAULT); else {}
    if (v.tuner)	/* Only 1 tuner */
      ret(-EINVAL);
    v.rangelow = 875 * 1600;
    v.rangehigh = 1080 * 1600;
    v.flags = VIDEO_TUNER_LOW;
    v.mode = VIDEO_MODE_AUTO;
    v.signal = 0xFFFF;	/* We can't get the signal strength */
    strcpy(v.name, "FM");
    if (copy_to_user(arg, v, sizeof(v)))
      ret(-EFAULT); else {}
    ret(0);
  }/*
  else if (cmd == VIDIOCSTUNER) {
    struct video_tuner v;
    if (copy_from_user(v, arg, sizeof(v)))
      ret(-EFAULT); else {}
    if (v.tuner != 0)
      ret(-EINVAL);
    ret(0);
  }
  else if(cmd == VIDIOCSFREQ) {
    if (copy_from_user(typhoon->curfreq, arg, sizeof(typhoon->curfreq)))
      ret(-EFAULT); else {}
    typhoon_setfreq(typhoon, typhoon->curfreq);
    ret(0);
  }*/
  return -ENOIOCTLCMD;
}
