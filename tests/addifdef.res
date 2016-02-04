#ifdef FOO
#ifdef LINUX_VERSION_CODE >= KERNEL_VERSION(3,5,0)
int one () {
  return 1;
}
#endif /* LINUX_VERSION_CODE >= KERNEL_VERSION(3,5,0) */
#endif /* LINUX_VERSION_CODE >= KERNEL_VERSION(3,6,1) */

#ifdef LINUX_VERSION_CODE >= KERNEL_VERSION(3,5,0)
/* comment about two */
int two () {
  return 2;
}
#endif /* LINUX_VERSION_CODE >= KERNEL_VERSION(3,5,0) */
