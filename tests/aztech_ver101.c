static int pcm20_ioctl(struct video_device *dev, unsigned int cmd, void *arg)
{
	struct pcm20_device *pcm20=dev->priv;
	int i;
	
	switch(cmd)
	{
		case VIDIOCGCAP:
		{
			struct video_capability v;
			v.type=VID_TYPE_TUNER;
			strcpy(v.name, "Miro PCM20");
			v.channels=1;
			v.audios=1;
			/* No we don't do pictures */
			v.maxwidth=0;
			v.maxheight=0;
			v.minwidth=0;
			v.minheight=0;
			if(copy_to_user(arg,&v,sizeof(v)))
				return -EFAULT;
			return 0;
		}
		case VIDIOCGTUNER:
		{
			struct video_tuner v;
			if(copy_from_user(&v, arg,sizeof(v))!=0) 
				return -EFAULT;
			if(v.tuner)	/* Only 1 tuner */ 
				return -EINVAL;
			v.rangelow=87*16000;
			v.rangehigh=108*16000;
			pcm20_getflags(pcm20, &v.flags/*, &v.signal*/);
			v.flags|=VIDEO_TUNER_LOW;
			v.mode=VIDEO_MODE_AUTO;
			strcpy(v.name, "FM");
			if(copy_to_user(arg,&v, sizeof(v)))
				return -EFAULT;
			return 0;
		}
		default:
			return -ENOIOCTLCMD;
	}
}
