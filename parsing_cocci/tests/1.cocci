@@
expression E;
@@

- #include <linux/wrapper.h>
  <...
(
- mem_map_reserve(E)
+ SetPageReserved(E)
|
- cs4x_mem_map_reserve(E)
+ SetPageReserved(E)
|
- mem_map_unreserve(E)
+ ClearPageReserved(E)
|
- cs4x_mem_map_unreserve(E)
+ ClearPageReserved(E)
)
  ...>
