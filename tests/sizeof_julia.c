static int typhoon_ioctl(struct video_device *dev, unsigned int cmd,
void *arg)
{
   copy_from_user(&v, arg, sizeof(v));
}
