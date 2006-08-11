@@
identifier ioctl, dev, cmd, arg;
// fresh identifier inode, file;
@@

  ioctl(
-       struct video_device *dev,
+       struct inode *inode, struct file *file,
	unsigned int cmd, void *arg) {
+   struct video_device *dev = video_devdata(file);
    ...
  }

@@
type T;
identifier v,fld;
@@

// copy_from_user and possibly copy_to_user

  ioctl(...) {
    <...
-     T v;
+     T *v = arg;
      ...
(
-     if (copy_from_user(&v,arg,sizeof(v))) return ...;
|
-     if (copy_from_user(&v,arg,sizeof(T))) return ...;
|
-     if (get_user(v,(T *)arg)) return ...;
)
      <...
(
-     v.fld
+     v->fld
|
-     &v
+     v
|
-     v
+     *v
)
      ...>
(
-    if (copy_to_user(arg,&v,sizeof(v))) return ...;
|
-    if (copy_to_user(arg,&v,sizeof(T))) return ...;
|
?-   if (put_user(v,(T *)arg)) return ...;
)
    ...>
  }

@@
type T;
identifier v,fld;
statement S;
@@

// copy_to_user only

  ioctl(...) {
    <...
-     T v;
+     T *v = arg;
      <...
(
-     v.fld
+     v->fld
|
-     &v
+     v
|
-     v
+     *v
)
      ...>
(
-     if (copy_to_user(arg,&v,sizeof(v))) return ...;
|
-     if (copy_to_user(arg,&v,sizeof(T))) return ...;
|
-     if (put_user(v,(T *)arg)) return ...;
)
    ...>
  }

@@
expression E;
statement S;
type T;
// fresh identifier tmp;
@@

// copy_from_user and copy_to_user that don't use a local variable

  ioctl(...) {
    <...
(
-     if (copy_from_user(&E,arg,sizeof(E))) return ...;
+     { unsigned long *tmp = arg; E = *tmp; }
|
-     if (copy_from_user(&E,arg,sizeof(T))) return ...;
+     { T *tmp = arg; E = *tmp; }
|
-     if (get_user(E,(T *)arg)) return ...;
+     { T *tmp = arg; E = *tmp; }
|
-     if (copy_to_user(arg,&E,sizeof(E))) return ...;
+     { unsigned long *tmp = arg; *tmp = E; }
|
-     if (copy_to_user(arg,&E,sizeof(T))) return ...;
+     { T *tmp = arg; *tmp = E; }
|
-     if (put_user(E,(T *)arg)) return ...;
+     { T *tmp = arg; *tmp = E; }
)
    ...>
  }
