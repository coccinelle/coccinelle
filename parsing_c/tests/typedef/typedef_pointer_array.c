// Hard to tell because Scsi_Device *device[xx]; could
// be a multiplication

struct x {

    ADV_SCSI_BIT_ID_TYPE init_tidmask;          /* Target init./valid mask */
    Scsi_Device          *device[ADV_MAX_TID+1]; /* Mid-Level Scsi Device */
};
