static int del_from_chain(struct ip_fw *volatile*chainptr, struct ip_fw *frwl)
{
	struct ip_fw 	*ftmp,*ltmp;
	unsigned short	tport1,tport2,tmpnum;
	char		matches,was_found;
	unsigned long 	flags;

	save_flags(flags);
	cli();

	ftmp=*chainptr;

	if ( ftmp == NULL )
	{
#ifdef DEBUG_IP_FIREWALL
		printk("ip_fw_ctl:  chain is empty\n");
#endif
		restore_flags(flags);
		return( EINVAL );
	}

	ltmp=NULL;
	was_found=0;

	while( !was_found && ftmp != NULL )
	{
		matches=1;
		if (ftmp->fw_src.s_addr!=frwl->fw_src.s_addr
		     ||  ftmp->fw_dst.s_addr!=frwl->fw_dst.s_addr
		     ||  ftmp->fw_smsk.s_addr!=frwl->fw_smsk.s_addr
		     ||  ftmp->fw_dmsk.s_addr!=frwl->fw_dmsk.s_addr
		     ||  ftmp->fw_via.s_addr!=frwl->fw_via.s_addr
		     ||  ftmp->fw_flg!=frwl->fw_flg)
        		matches=0;

		tport1=ftmp->fw_nsp+ftmp->fw_ndp;
		tport2=frwl->fw_nsp+frwl->fw_ndp;
		if (tport1!=tport2)
		        matches=0;
		else if (tport1!=0)
		{
			for (tmpnum=0;tmpnum < tport1 && tmpnum < IP_FW_MAX_PORTS;tmpnum++)
        		if (ftmp->fw_pts[tmpnum]!=frwl->fw_pts[tmpnum])
				matches=0;
		}
		if (strncmp(ftmp->fw_vianame, frwl->fw_vianame, IFNAMSIZ))
		        matches=0;
		if(matches)
		{
			was_found=1;
			if (ltmp)
			{
				ltmp->fw_next=ftmp->fw_next;
				kfree(ftmp);
				ftmp=ltmp->fw_next;
        		}
      			else
      			{
      				*chainptr=ftmp->fw_next;
	 			kfree(ftmp);
				ftmp=*chainptr;
			}
		}
		else
		{
			ltmp = ftmp;
			ftmp = ftmp->fw_next;
		 }
	}
	restore_flags(flags);
	if (was_found)
		return 0;
	else
		return(EINVAL);
}
