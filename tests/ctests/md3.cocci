// leads to triplication of I under coccinelle e5e5c508
//FIXME: rename example s/wishlist/problem/g.
@@
identifier I;
identifier id;
field list[n] fs;
@@
struct id { fs
-double  I[3];
++double3 I;
  ...
}
