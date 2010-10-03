@@
local function ioctlfn;
identifier dev, cmd, arg;
identifier v;
type T;
statement S1, S2;
identifier fld;
@@

-     if (copy_from_user(v,arg,sizeof(v)) != 0) S1 else {}
