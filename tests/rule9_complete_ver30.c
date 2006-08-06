int
cciss_scsi_proc_info(char *buffer, /* data buffer */
		char **start, 	   /* where data in buffer starts */
		off_t offset,	   /* offset from start of imaginary file */
		int length, 	   /* length of data in buffer */
		int hostnum, 	   /* which host adapter (always zero for me) */
		int func)	   /* 0 == read, 1 == write */
{

	int buflen, datalen;
	struct Scsi_Host *sh;
	ctlr_info_t *ci;
	int cntl_num;


	sh = scsi_host_hn_get(hostnum);
	if (sh == NULL) /* This really shouldn't ever happen. */
		return -EINVAL;

	ci = (ctlr_info_t *) sh->hostdata[0];
	if (ci == NULL)  /* This really shouldn't ever happen. */
		return -EINVAL;

	cntl_num = ci->ctlr;	/* Get our index into the hba[] array */

	if (func == 0) {	/* User is reading from /proc/scsi/ciss*?/?*  */
		buflen = sprintf(buffer, "hostnum=%d\n", hostnum); 	

		datalen = buflen - offset;
		if (datalen < 0) { 	/* they're reading past EOF. */
			datalen = 0;
			*start = buffer+buflen;	
		} else
			*start = buffer + offset;
		return(datalen);
	} else 	/* User is writing to /proc/scsi/cciss*?/?*  ... */
		return cciss_scsi_user_command(cntl_num, hostnum,
			buffer, length);	
} 
