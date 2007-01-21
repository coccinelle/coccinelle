// We could drop variables for which the only use is in E, but we don't have
// any way of expressing this
@@
expression A, C, D, E;
@@

- usb_storage_raw_bulk(A, SCSI_DATA_WRITE, C, D, E, F)
+ usb_stor_bulk_transfer_buf(A,A->send_bulk_pipe,C,D,NULL)

@@
expression A, C, D, E;
@@

- usb_storage_raw_bulk(A, SCSI_DATA_READ, C, D, E, F)
+ usb_stor_bulk_transfer_buf(A,A->recv_bulk_pipe,C,D,NULL)

// the SP needs to be extended to deal with more of these return value
// conversion issues
@@
expression A, B, C, D, E, F, G, H;
@@

- usb_storage_send_control(A,B,C,D,E,F,G,H) != US_BULK_TRANSFER_GOOD
+ usb_stor_ctrl_transfer(A,B,C,D,E,F,G,H) != USB_STOR_XFER_GOOD

@@
expression A, B, C, D, E, F, G, H;
@@

- usb_storage_send_control(A,B,C,D,E,F,G,H)
+ usb_stor_ctrl_transfer(A,B,C,D,E,F,G,H)

@@
expression A;
struct ScsiCmnd *X;
identifier B, C, D, E;
@@

- usb_storage_bulk_transport(A,X->B,X->C,X->D,X->E)
+ usb_stor_bulk_transfer_srb(A,(X->B==SCSI_DATA_WRITE ?
+                            A->send_bulk_pipe : A->recv_bulk_pipe),X,X->D)

@@
expression A, C, D;
@@

- usb_storage_bulk_transport(A,SCSI_DATA_WRITE,C,D,0)
+ usb_stor_bulk_transfer_buf(A,A->send_bulk_pipe,C,D,NULL)

@@
expression A, C, D;
@@

- usb_storage_bulk_transport(A,SCSI_DATA_READ,C,D,0)
+ usb_stor_bulk_transfer_buf(A,A->recv_bulk_pipe,C,D,NULL)

@@
expression A, C, D, E;
@@

- usb_storage_bulk_transport(A,SCSI_DATA_WRITE,C,D,E)} becomes
+ usb_stor_bulk_transfer_sg(A,A->send_bulk_pipe,C,D,E,NULL)

@@
expression A, C, D, E;
@@

- usb_storage_bulk_transport(A,SCSI_DATA_READ,C,D,E)} becomes
+ usb_stor_bulk_transfer_sg(A,A->recv_bulk_pipe,C,D,E,NULL)
