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
- mem_map_unreserve(E)
+ ClearPageReserved(E)
|
- cs4x_mem_map_unreserve(E)
+ ClearPageReserved(E)
|
- cs4x_mem_map_reserve(E)
+ SetPageReserved(E)
)


// now that handle #define, not needed anymore
//- cs4x_mem_map_unreserve(E)
//+ ClearPageReserved(E)
//- cs4x_mem_map_reserve(E)
//+ SetPageReserved(E)


// @@ identifier page; @@
// 
// - #define cs4x_mem_map_unreserve(page) mem_map_unreserve(page)
// 
// @@ @@
// 
// - #define cs4x_mem_map_reserve(page) mem_map_reserve(page)
// 
// // pourrait meme faire 
// @@ @@
// 
// - #define     MAP_NR(x)       virt_to_page(x)
// 
// @@ expression X; @@ 
// - MAP_NR(X)
// + virt_to_page(X) 
