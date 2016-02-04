@@
identifier usb;
@@

<ooo
usb_register(&usb)
ooo
usb_deregister(&usb)
ooo>

@@
local function probe_fn;
local function disconnect_fn;
int minor;
@@

struct usb_driver usb = {
  ooo
  probe:          probe_fn,
  ooo
  disconnect:     disconnect_fn,
  ooo
  minor:          minor,
  ooo
};

@@
int minor_offset;
expression E1, E2, E3, E4, E5, E6;
type T;
T moe;
identifier field;
identifier v;
expression L1, L2, L3, L;
statement loop_body;
@@

(
  probe_fn(...) {
    ...
    int v;
    ...    WHEN != v = L
+   if (usb_register_dev(&usb, 1, &v)) {
      while(L1) {
        <...
        v = L2;
        ...>
      }
+   }
    ...    WHEN != v = L
!   moe.field = v;
    ...    WHEN != v = L
    devfs_register(E1, E2, E3, USB_MAJOR, minor + moe.field, E4, E5, E6)
    ...    WHEN != v = L
  }
|
  probe_fn(...) {
    ...
    int v;
    ...    WHEN != v = L
+   if (usb_register_dev(&usb, 1, &v)) {
      while(L1) {
        <...
        v = L2;
        ...>
      }
+   }
    ...    WHEN != v = L
(
!   moe.field = v;
    ooo    WHEN != v = L
    devfs_register(E1, E2, E3, USB_MAJOR, minor + v, E4, E5, E6)
)
    ...    WHEN != v = L
  }
|
  probe_fn(...) {
    ...    WHEN != moe.field = L
+   if (usb_register_dev(&usb, 1, &moe.field)) {
      while(L1) {
        <...
        moe.field = L2;
        ...>
      }
+   }
    ...    WHEN != moe.field = L
    devfs_register(E1, E2, E3, USB_MAJOR, minor + moe.field, E4, E5, E6)
    ...    WHEN != moe.field = L
  }
)

@@
T E;
identifier f;
@@

  disconnect_fn(...) {
    ***
!   devfs_unregister(E.f)
+   usb_deregister_dev(&usb, 1, E.field)
    ***
  }
