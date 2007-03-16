#define SC_FCMND(fcmnd) ((struct scsi_cmnd *)((long)fcmnd - (long)&(((struct scsi_cmnd *)0)->SCp)))
