@@
local function ioctlfn;
identifier dev, cmd, arg;
fresh identifier i, f;
identifier v;
type T;
statement S1, S2;
identifier fld;
@@
  ioctlfn(
-       struct video_device *dev,
+       struct inode *i, struct file *f,
        unsigned int cmd, void *arg) {
    <...
    {
      ...
      T v;
      ...
-     if (copy_from_user(&v,arg,sizeof(v)) != 0) S1
      <...
-     v.fld
+     v->fld
      ...>
?-    if (copy_to_user(&v,arg,sizeof(v))) S2
      ...
    }
    ...>
  }
