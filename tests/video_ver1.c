static int typhoon_ioctl(struct video_device *dev, unsigned int cmd, void *arg)
{
	struct typhoon_device *typhoon = dev->priv;

	switch (cmd) {
	case VIDIOCGTUNER:
		{
			struct video_tuner v;
			return 0;
		}
	case VIDIOCSTUNER:
		{
			struct video_tuner v;
			return 0;
		}
	case VIDIOCSFREQ:
		return 0;
	default:
		return -ENOIOCTLCMD;
	}
}
