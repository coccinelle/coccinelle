static int pcm20_ioctl(struct video_device *dev, unsigned int cmd, void *arg)
{
	struct video_tuner v;
	pcm20_getflags(pcm20, &v.flags, &v.xxx, &v.signal);
}
