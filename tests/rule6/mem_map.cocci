// should also do the following, but probably not supported
//- #include <linux/wrapper.h>
//- #include "drivers/sound/cs4281/cs4281\_wrapper.h"

@@
expression E;
@@
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
