@ sk_data_ready_assigned @
struct sock *sk;
identifier drv_data_ready;
@@

        sk->sk_data_ready = drv_data_ready;

@ sk_data_ready_declared depends on sk_data_ready_assigned @
identifier sk;
identifier sk_data_ready_assigned.drv_data_ready;
fresh identifier backport_drv_data_ready = "backport_" ## drv_data_ready;
@@

+#if LINUX_VERSION_CODE < KERNEL_VERSION(3,15,0)
+static void backport_drv_data_ready(struct sock *sk, int unused)
+{
+       drv_data_ready(sk);
+}
+#else
+
drv_data_ready(struct sock *sk)
{
        ...
}
+#endif
