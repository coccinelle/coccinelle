int cpqfcTS_proc_info (char *buffer, char **start, off_t offset, int length, 
		       int hostno, int inout)
{
  struct Scsi_Host *host;
  Scsi_Cmnd DumCmnd;
  int Chan, Targ, i;
  struct info_str info;
  CPQFCHBA *cpqfcHBA;
  PTACHYON fcChip;
  PFC_LOGGEDIN_PORT pLoggedInPort;
  char buf[81];

  // Search the Scsi host list for our controller
  host = scsi_host_hn_get(hostno);

  if (!host) return -ESRCH;

  if (inout) return -EINVAL;

  // get the pointer to our Scsi layer HBA buffer  
  cpqfcHBA = (CPQFCHBA *)host->hostdata;
  fcChip = &cpqfcHBA->fcChip;
  
  *start 	  = buffer;

  info.buffer     = buffer;
  info.buflength  = length;
  info.bufoffset  = offset;
  info.filpos     = 0;
  info.buffillen  = 0;
  copy_info(&info, "Driver version = %d.%d.%d", VER_MAJOR, VER_MINOR, VER_SUBMINOR); 
  cpqfcTSDecodeGBICtype( &cpqfcHBA->fcChip, &buf[0]);
  cpqfcTSGetLPSM( &cpqfcHBA->fcChip, &buf[ strlen(buf)]);
  copy_info(&info, "%s\n", buf); 

#define DISPLAY_WWN_INFO
#ifdef DISPLAY_WWN_INFO
  copy_info(&info, "WWN database: (\"port_id: 000000\" means disconnected)\n");
  for ( Chan=0; Chan <= host->max_channel; Chan++) {
    DumCmnd.channel = Chan;
    for (Targ=0; Targ <= host->max_id; Targ++) {
      DumCmnd.target = Targ;
      if ((pLoggedInPort = fcFindLoggedInPort( fcChip,
	    			&DumCmnd, // search Scsi Nexus
    				0,        // DON'T search list for FC port id
    				NULL,     // DON'T search list for FC WWN
    				NULL))){   // DON'T care about end of list
	copy_info(&info, "Host: scsi%d Channel: %02d TargetId: %02d -> WWN: ",
			   hostno, Chan, Targ);
        for( i=3; i>=0; i--)        // copy the LOGIN port's WWN
          copy_info(&info, "%02X", pLoggedInPort->u.ucWWN[i]);
        for( i=7; i>3; i--)             // copy the LOGIN port's WWN
          copy_info(&info, "%02X", pLoggedInPort->u.ucWWN[i]);
	copy_info(&info, " port_id: %06X\n", pLoggedInPort->port_id); 
      }
    }
  }
#endif



  
  
// Unfortunately, the proc_info buffer isn't big enough
// for everything we would like...
// For FC stats, compile this and turn off WWN stuff above  
//#define DISPLAY_FC_STATS
#ifdef DISPLAY_FC_STATS
// get the Fibre Channel statistics
  {
    int DeltaSecs = (jiffies - cpqfcHBA->fcStatsTime) / HZ;
    int days,hours,minutes,secs;
    
    days = DeltaSecs / (3600*24); // days
    hours = (DeltaSecs% (3600*24)) / 3600; // hours
    minutes = (DeltaSecs%3600 /60); // minutes
    secs =  DeltaSecs%60;  // secs
copy_info( &info, "Fibre Channel Stats (time dd:hh:mm:ss %02u:%02u:%02u:%02u\n",
      days, hours, minutes, secs);
  }
    
  cpqfcHBA->fcStatsTime = jiffies;  // (for next delta)

  copy_info( &info, "  LinkUp           %9u     LinkDown      %u\n",
        fcChip->fcStats.linkUp, fcChip->fcStats.linkDown);
        
  copy_info( &info, "  Loss of Signal   %9u     Loss of Sync  %u\n",
    fcChip->fcStats.LossofSignal, fcChip->fcStats.LossofSync);
		  
  copy_info( &info, "  Discarded Frames %9u     Bad CRC Frame %u\n",
    fcChip->fcStats.Dis_Frm, fcChip->fcStats.Bad_CRC);

  copy_info( &info, "  TACH LinkFailTX  %9u     TACH LinkFailRX     %u\n",
    fcChip->fcStats.linkFailTX, fcChip->fcStats.linkFailRX);
  
  copy_info( &info, "  TACH RxEOFa      %9u     TACH Elastic Store  %u\n",
    fcChip->fcStats.Rx_EOFa, fcChip->fcStats.e_stores);

  copy_info( &info, "  BufferCreditWait %9uus   TACH FM Inits %u\n",
    fcChip->fcStats.BB0_Timer*10, fcChip->fcStats.FMinits );
	
  copy_info( &info, "  FC-2 Timeouts    %9u     FC-2 Logouts  %u\n",
    fcChip->fcStats.timeouts, fcChip->fcStats.logouts); 
        
  copy_info( &info, "  FC-2 Aborts      %9u     FC-4 Aborts   %u\n",
    fcChip->fcStats.FC2aborted, fcChip->fcStats.FC4aborted);
   
  // clear the counters
  cpqfcTSClearLinkStatusCounters( fcChip);
#endif
	
  return info.buffillen;
}
