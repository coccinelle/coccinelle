@@
identifier I;
identifier f;
@@

struct file_operations I = {
  .ioctl = f
};

@@
expression A, B, C;
identifier inodename, filename, cmd, arg;
@@

f(struct inode *inodename, struct file *filename, unsigned cmd,
  unsigned long arg) {
   <...
-  scsi_cmd_ioctl(A, cmd, C)
+  scsi_cmd_ioctl(filename, A, cmd, C)
   ...>
}

// the file should have no other calls to scsi_cmd_ioctl

@@
identifier I;
identifier f;
@@

struct block_device_operations I = {
  .ioctl = f
};

@@
expression A, B, C;
identifier inodename, filename, cmd, arg;
@@

f(struct inode *inodename, struct file *filename, unsigned cmd,
  unsigned long arg) {
   <...
(
-  cdrom_ioctl(A, inodename, cmd, arg)
+  cdrom_ioctl(filename, A, inodename, cmd, arg)
|
-  generic_ide_ioctl(A, cmd, arg)
+  generic_ide_ioctl(filename, A, cmd, arg)
|
-  scsi_cmd_ioctl(A, cmd, C)
+  scsi_cmd_ioctl(filename, A, cmd, C)
)
   ...>
}

// the file should have no other calls to cdrom_ioctl or generic_ide_ioctl
