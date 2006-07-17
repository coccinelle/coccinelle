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
-     T v;
+     T *v;
      ...
-     if (copy_from_user(v,arg,size_of(v)) != 0) S1 else {}
      <...
-     v.fld
+     v->fld
      ...>
?-    if (copy_to_user(arg,v,size_of(v))) S2 else {}
      ...
    }
    ...>
  }
