//drivers/net/wireless/ath/carl9170/usb.c
//drivers/net/wireless/p54/p54usb.c
//drivers/net/wireless/ath/ath9k/hif_usb.c

@@
identifier backport_driver;
@@
struct usb_driver backport_driver = {
+#if (LINUX_VERSION_CODE >= KERNEL_VERSION(3,5,0))
.disable_hub_initiated_lpm = 1,
+#endif
};

@@
identifier backport_driver;
@@
struct usb_driver backport_driver = {
+#if (LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,27))
        .soft_unbind = 1,
+#endif
};
