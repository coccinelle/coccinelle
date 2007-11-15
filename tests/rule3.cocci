@@
// struct us_data *E1;
// struct us_data E1;
expression E1;
expression E2;
@@

//- usb_stor_clear_halt(E1->pusb_dev,E2)
//- usb_stor_clear_halt(E1.pusb_dev,E2)
-  usb_clear_halt(E1->pusb_dev, E2)
+  usb_stor_clear_halt(E1, E2)

//error words = [usb_clear_halt]
