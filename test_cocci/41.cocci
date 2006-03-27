@@
expression E1, E2, E3;
@@

- usb_deregister_dev(E1, E2, E3);
+ usb_deregister_dev(E2, E3);

@@
struct usb_driver d;
!expression fops;
@@

- d.fops = fops;

@@
struct usb_driver d;
!int minor;
@@

- d.minor = minor;

@@
struct usb_driver d;
int num_minor;
@@

- d.num_minor = num_minor;

@@
expression E1, E2, E3;
identifier ret;
statement S;
@@

- ret = usb_register_dev(E1, E2, E3);
+ ret = usb_register_dev(fops, minor, E2, E3);
- if (ret) {
-   if (ret != -ENODEV) S
-   ...
- }
+ if (ret) S
