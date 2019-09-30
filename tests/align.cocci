// spatch -test align --macro-file-builtins macros.h

@@
field list fs;
@@

struct foo { fs };
+ struct bar { fs };
