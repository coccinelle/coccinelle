@@
expression E1, E2, E3;
@@

- usb_deregister_dev(E1, E2, E3);
+ usb_deregister_dev(E2, E3);

@@
struct usb_driver d;
!expression fops_val;
@@

- d.fops = fops_val;

@@
struct usb_driver d;
!int minor_val;
@@

- d.minor = minor_val;

@@
struct usb_driver d;
int num_minor_val;
@@

- d.num_minor = num_minor_val;

@@
expression E1, E2, E3;
identifier ret;
statement S;
@@

- ret = usb_register_dev(E1, E2, E3);
+ ret = usb_register_dev(fops_val, minor_val, E2, E3);
- if (ret) {
-   if (ret != -ENODEV) S
-   ...
- }
+ if (ret) S
