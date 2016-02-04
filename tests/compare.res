struct aic7xxx_host {
  struct aic7xxx_cmd_queue {
    struct scsi_cmnd *head;
    struct scsi_cmnd *tail;
  } completeq;
  unsigned char  msg_buf[13];      /* The message for the target */
  unsigned char  msg_type;
#define MSG_TYPE_NONE              0x00
#define MSG_TYPE_INITIATOR_MSGOUT  0x01
#define MSG_TYPE_INITIATOR_MSGIN   0x02
  unsigned char  msg_len;          /* Length of message */
  unsigned char  msg_index;        /* Index into msg_buf array */
};

