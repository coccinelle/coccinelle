@@
local function ioctlfn;
identifier dev, cmd, arg;
//fresh identifier i, f;
identifier v;
type T;
statement S1, S2;
identifier fld;
expression E;
statement S;
@@

//  ioctlfn(
//        struct video_device *dev,
//        unsigned int cmd, void *arg) {
//    <...
    if (cmd == E) {
      ...
-     if (copy_from_user(v,arg,size_of(v)) != 0) S1
      ...
-     if (copy_to_user(arg,v,size_of(v))) S2
      ...
    }
    else { ... }
//    ...>
//  }
