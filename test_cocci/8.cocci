@@
expression intf, X;
@@

  #include <linux/usb.h>
  <...
(
- dev_get_drvdata(&intf->dev)
+ usb_get_intfdata(intf)
|
- dev_set_drvdata(&intf->dev, X)
+ usb_set_intfdata(intf, X)
)
  ...>
