static int check_media_type;
/* automatically restart mrw format */
static int mrw_format_restart = 1;
module_param(debug, bool, 0);
module_param(autoclose, bool, 0);



int register_cdrom(struct cdrom_device_info *cdi)
{
	static char banner_printed;
        struct cdrom_device_ops *cdo = cdi->ops;
        int *change_capability = (int *)&cdo->capability; /* hack */

	cdinfo(CD_OPEN, "entering register_cdrom\n"); 

	if (cdo->open == NULL || cdo->release == NULL)
		return -2;
	if (!banner_printed) {
		printk(KERN_INFO "Uniform CD-ROM driver " REVISION "\n");
		banner_printed = 1;
#ifdef CONFIG_SYSCTL
		cdrom_sysctl_register();
#endif /* CONFIG_SYSCTL */ 
	}

	ENSURE(drive_status, CDC_DRIVE_STATUS );
	ENSURE(media_changed, CDC_MEDIA_CHANGED);
	ENSURE(tray_move, CDC_CLOSE_TRAY | CDC_OPEN_TRAY);
	ENSURE(lock_door, CDC_LOCK);
	ENSURE(select_speed, CDC_SELECT_SPEED);
	ENSURE(get_last_session, CDC_MULTI_SESSION);
	ENSURE(get_mcn, CDC_MCN);
	ENSURE(reset, CDC_RESET);
	ENSURE(audio_ioctl, CDC_PLAY_AUDIO);
	ENSURE(dev_ioctl, CDC_IOCTLS);
	ENSURE(generic_packet, CDC_GENERIC_PACKET);
	cdi->mc_flags = 0;
	cdo->n_minors = 0;
        cdi->options = CDO_USE_FFLAGS;
	
	if (autoclose==1 && CDROM_CAN(CDC_CLOSE_TRAY))
		cdi->options |= (int) CDO_AUTO_CLOSE;
	if (autoeject==1 && CDROM_CAN(CDC_OPEN_TRAY))
		cdi->options |= (int) CDO_AUTO_EJECT;
	if (lockdoor==1)
		cdi->options |= (int) CDO_LOCK;
	if (check_media_type==1)
		cdi->options |= (int) CDO_CHECK_TYPE;

	if (CDROM_CAN(CDC_MRW_W))
		cdi->exit = cdrom_mrw_exit;

	if (cdi->disk)
		cdi->cdda_method = CDDA_BPC_FULL;
	else
		cdi->cdda_method = CDDA_OLD;

	if (!cdo->generic_packet)
		cdo->generic_packet = cdrom_dummy_generic_packet;

	cdinfo(CD_REG_UNREG, "drive \"/dev/%s\" registered\n", cdi->name);
	spin_lock(&cdrom_lock);
	cdi->next = topCdromPtr; 	
	topCdromPtr = cdi;
	spin_unlock(&cdrom_lock);
	return 0;
}


int unregister_cdrom(struct cdrom_device_info *unreg)
{
	struct cdrom_device_info *cdi, *prev;
	cdinfo(CD_OPEN, "entering unregister_cdrom\n"); 

	prev = NULL;
	spin_lock(&cdrom_lock);
	cdi = topCdromPtr;
	while (cdi && cdi != unreg) {
		prev = cdi;
		cdi = cdi->next;
	}

	if (cdi == NULL) {
		spin_unlock(&cdrom_lock);
		return -2;
	}
	if (prev)
		prev->next = cdi->next;
	else
		topCdromPtr = cdi->next;

	spin_unlock(&cdrom_lock);

	if (cdi->exit)
		cdi->exit(cdi);

	cdi->ops->n_minors--;
	cdinfo(CD_REG_UNREG, "drive \"/dev/%s\" unregistered\n", cdi->name);
	return 0;
}
