@exists@
local idexpression x;
statement S;
constant C;
@@

* x = kmalloc(...)
...
if (x == NULL) S
... when != x
    when != if (...) { <+...x...+> }
* return \(-C\|NULL\);
