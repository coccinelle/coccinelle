@@
local function ioctlfn;
identifier dev, cmd, arg;
//fresh identifier i, f;
identifier v;
type T;
identifier fld;
expression E1, E2;
@@

  ioctlfn(
-       struct video_device *dev,
+       struct inode *i, struct file *f,
        unsigned int cmd, void *arg) {
    <...
-     T v;
+     T *v;
      ...
-     if (copy_from_user(&v,arg,sizeof(v)) != 0) return E1;
      <...
-     v.fld
+     v->fld
      ...>
?-    if (copy_to_user(arg,&v,sizeof(v))) return E2;
    ...>
  }
