#define SC_FCMND(fcmnd) ((Scsi_Cmnd *)((long)fcmnd - (long)&(((Scsi_Cmnd *)0)->SCp)))

int main() {
  return ((Scsi_Cmnd *)((long)fcmnd - (long)&(((Scsi_Cmnd *)0)->SCp)));
}
