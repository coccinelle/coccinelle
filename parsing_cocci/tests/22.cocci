@@
@@

--- a/drivers/usb/storage/...
+++ b/drivers/usb/storage/...

@@
struct us_data X;
expression Y;
@@

- usb_sndbulkpipe (X->pusb_dev, Y)
+ X->send_bulk_pipe

@@
struct us_data X;
expression Y;
@@

- usb_rcvbulkpipe (X->pusb_dev, Y)
+ X->recv_bulk_pipe

@@
struct us_data X;
expression Y;
@@

- usb_sndctrlpipe (X->pusb_dev, Y)
+ X->send_ctrl_pipe

@@
struct us_data X;
expression Y;
@@

- usb_rcvctrlpipe (X->pusb_dev, Y)
+ X->recv_ctrl_pipe

@@
@@

error words = [usb_sndbulkpipe, usb_rcvbulkpipe, usb_sndctrlpipe, usb_rcvctrlpipe]
