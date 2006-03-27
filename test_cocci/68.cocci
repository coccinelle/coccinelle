@@
struct map_info X;
constant {8,16,32} n;
@@

- X->write##n(...)
+ map_write##n(...)

@@
struct map_info X;
constant {8,16,32} n;
@@

- X->read##n(...)
+ map_read##n(...)

@@
struct map_info X;
@@

- X->copy_from(...)
+ map_copy_from(...)

@@
struct map_info X;
@@

- X->copy_to(...)
+ map_copy_to(...)
