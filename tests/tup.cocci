@ rule1 @
identifier I;
identifier f;
@@

struct block_device_operations I = {
  .ioctl = f,
};

@@
identifier rule1.f;
expression A, B, C;
identifier inodename, filename, cmd, arg;
@@

f(struct inode *inodename, struct file *filename, unsigned int cmd,
  unsigned long arg) {
   <...
-  cdrom_ioctl(A, inodename, cmd, arg)
+  xxx()
   ...>
}
