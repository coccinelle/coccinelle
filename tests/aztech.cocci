@@
identifier ioctl, dev, cmd, arg;
// fresh identifier inode, file;
@@

  ioctl(
-       struct video_device *dev,
+       struct inode *inode, struct file *file,
	int cmd, void *arg) { // cmd was unsigned int
+   struct video_device *dev = video_devdata(file);
    ...
  }

//@@
//type T;
//identifier v,fld;
//statement S1, S2;
//@@
//
//// copy_from_user and possibly copy_to_user
//
//  ioctl(...) {
//    <...
//      T v;
////+   T *v;
//      ...
//-     if (copy_from_user(&v,arg,sizeof(v))) S1
//      <...
//(
//-     v.fld
//+     v->fld
//|
//-     &v
//+     v
//|
//-     v
//+     *v
//)
//      ...>
//?-    if (copy_to_user(arg,&v,sizeof(v))) S2
//    ...>
//  }

@@
type T;
identifier v,fld;
statement S;
@@

// copy_to_user only

  ioctl(...) {
    <...
      T v;
//+   T *v;
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
-     if (copy_to_user(arg,&v,sizeof(v))) return ...;
    ...>
  }

//@@
//expression E;
//statement S;
//// fresh identifier tmp;
//@@
//
//// copy_from_user and copy_to_user that don't use a local variable
//
//  ioctl(...) {
//    <...
//(
//-     if (copy_from_user(&E,arg,sizeof(E))) S
//+     { unsigned long *tmp = arg; E = *tmp; }
//|
//-     if (copy_to_user(arg,&E,sizeof(E))) S
//+     { unsigned long *tmp = arg; *tmp = E; }
//)
//    ...>
//  }
//