
static int cmm_ioctl()
{
	struct pcmcia_device *link;
	char *ioctl_names[CM_IOC_MAXNR + 1] = {
		[_IOC_NR(CM_IOSDBGLVL)] "CM4000_DBGLVL",
	};

}
