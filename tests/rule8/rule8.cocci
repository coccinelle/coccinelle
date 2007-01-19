@@
struct usb_interface *intf;
expression E;
@@

(
- dev_get_drvdata(&intf->dev)
+ usb_get_intfdata(intf)
|
// bug fix, but seems reasonable
- dev_set_drvdata(&intf->dev, 0)
+ usb_set_intfdata(intf,NULL)
|
- dev_set_drvdata(&intf->dev, E)
+ usb_set_intfdata(intf,E)
)
