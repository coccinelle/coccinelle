int cpqfcTS_queuecommand(Scsi_Cmnd *Cmnd, void (* done)(Scsi_Cmnd *))
{
  struct Scsi_Host *HostAdapter = Cmnd->host;
  CPQFCHBA *cpqfcHBAdata = (CPQFCHBA *)HostAdapter->hostdata;
  PTACHYON fcChip = &cpqfcHBAdata->fcChip;
  TachFCHDR_GCMND fchs;  // only use for FC destination id field  
  PFC_LOGGEDIN_PORT pLoggedInPort;
  ULONG ulStatus, SESTtype;
  LONG ExchangeID;




  ENTER("cpqfcTS_queuecommand");
      
  PCI_TRACEO( (ULONG)Cmnd, 0x98)
      
  
  Cmnd->scsi_done = done;
#ifdef DEBUG_CMND  
  cpqfcTS_print_scsi_cmd( Cmnd);
#endif
}
