@@
struct file_operations file_ops;
local function fn;
@@

file_ops.ioctl = fn;

@@
expression E1, E2, E3;
identifier X, fl;
type T;
@@

fn(T X, struct file *fl, ...) {
  <...
- scsi_cmd_ioctl(E1, E2, E3);
+ scsi_cmd_ioctl(fl, E1, E2, E3);
  ...>
}

@@
struct block_device_operations block_ops;
local function fn;
@@

block_ops.ioctl = fn;

@@
expression E1, E2, E3;
identifier X, fl;
type T;
@@

fn(T X, struct file *fl, ...) {
  <...
- cdrom_ioctl(E1, E2, E3);
+ cdrom_ioctl(fl, E1, E2, E3);
  ...>
}

@@
expression E1, E2, E3;
identifier X, fl;
type T;
@@

fn(T X, struct file *fl, ...) {
  <...
- generic_ide_ioctl(E1, E2, E3);
+ generic_ide_ioctl(fl, E1, E2, E3);
  ...>
}
