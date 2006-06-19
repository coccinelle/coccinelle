@@
+struct urb u1;
struct urb u2;
expression E, E2;
@@

  <***
  u1.transfer_flags = URB_ASYNC_UNLINK|E;
  ***   WHEN != u1.transfer_flags = E2;
    <...
-   usb_unlink_urb(u2)
+   usb_kill_urb(u2)
    ...>
  ***>
