static int typhoon_ioctl(struct video_device *dev, unsigned int cmd, void *arg)
{
  if (copy_from_user(v,arg) != 0) 
    return -EFAULT; else {}
}
