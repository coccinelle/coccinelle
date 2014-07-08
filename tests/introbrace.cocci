@@
identifier ret;
@@

ret = request_threaded_irq(...);
+ret = compat_request_threaded_irq(12);
