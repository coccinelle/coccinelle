@@
local function ioctlfn;
identifier dev, cmd, arg;
@@

  ioctlfn(
-       struct video_device *dev,
+       struct inode *i, struct file *f,
        unsigned int cmd, void *arg) {
    ...
  }
