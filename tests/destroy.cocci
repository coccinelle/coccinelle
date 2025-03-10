@@
expression irq, dev;
@@

+#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,31)
free_irq(irq, dev);
+#else
+compat_free_threaded_irq(&private->irq_compat);
+compat_destroy_threaded_irq(&dev->irq_compat);
+#endif
