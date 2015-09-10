@@
identifier main;
@@

+#ifdef LINUX_VERSION_CODE >= KERNEL_VERSION(3,5,0)
int main() { ... }
+#endif /* LINUX_VERSION_CODE >= KERNEL_VERSION(3,5,0) */
