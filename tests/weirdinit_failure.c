
static int cmm_ioctl()
{
	dev_link_t *link;
	char *ioctl_names[CM_IOC_MAXNR + 1] = {
	  [_IOC_NR(CM_IOSDBGLVL)] "CM4000_DBGLVL",
	};

}
