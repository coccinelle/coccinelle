static int typhoon_ioctl(struct video_device *dev, unsigned int cmd,
void *arg)
{
   if (cmd == VIDIOCGTUNER) {
     struct video_tuner v;
     if (copy_from_user(v, arg, sizeof(v)) != 0)
       ret(-EFAULT); else {}
   }
}
