@@
local function ioctlfn;
identifier dev, cmd, arg;
identifier v;
type T;
statement S1, S2;
identifier fld;
expression E1,E2;
@@
-     if (copy_from_user(&v,arg,sizeof(v)) != 0) return E1;
      ...
?-    if (copy_to_user(&v,arg,sizeof(v))) return E2;
