@@
expression E;
@@

(
- usb_dec_dev_use(X)
+ usb_put_dev(X)
|
- usb_inc_dev_use(X)
+ usb_get_dev(X)
)
