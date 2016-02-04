@@
local function ioctlfn;
identifier dev, cmd, arg;
identifier v;
type T;
identifier fld;
@@

  ioctlfn(
        struct video_device *dev,
        unsigned int cmd, void *arg) {
    <...
     {
      ...
-     T v;
+     T *v;
      ...
-     if (copy_from_user(v,arg,sizeof(v)) != 0) return ...;
      <...
-     v.fld
+     v->fld
      ...>
?-    if (copy_to_user(arg,v,sizeof(v))) return ...;
      ...
     }
    ...>
  }
