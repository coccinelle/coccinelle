//int (*done)[4];
//int *done[4];

void f() {
	hpsb_set_packet_complete_task(packet, (void (*)(void*))sbp2_free_packet,
				      packet);
}


//int (*vtob (int));
//int (*vtob) (int);
//int *vtob[VTOB_HASH_SIZE];

int (*vtob[VTOB_HASH_SIZE]); // they parse the same :( => ambiguity
int *(vtob[VTOB_HASH_SIZE]);

//int *vtob[VTOB_HASH_SIZE];


void (*done)(Scsi_Cmnd *);

static int usb_storage_queuecommand( Scsi_Cmnd *srb , void (*done)(Scsi_Cmnd *))
{
	struct us_data *us = (struct us_data *)srb->device->host->hostdata[0];
	int state = atomic_read(&us->sm_state);

}



static const char* usb_storage_info(struct Scsi_Host *host)
{
	return "SCSI emulation for USB Mass Storage devices";
}
