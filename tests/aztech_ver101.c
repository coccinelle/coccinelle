static int pcm20_ioctl(struct video_device *dev, unsigned int cmd, void *arg)
{
	struct pcm20_device *pcm20=dev->priv;
	int i;
	
	switch(cmd)
	{
		case VIDIOCGCAP:
		{
			struct video_tuner v;
			if(copy_from_user(&v, arg,sizeof(v))!=0) 
				return -EFAULT;
			if(v.tuner)	/* Only 1 tuner */ 
				return -EINVAL;
			if(copy_to_user(arg,&v, sizeof(v)))
				return -EFAULT;
			return 0;
		}
		default:
			return -ENOIOCTLCMD;
	}
}
