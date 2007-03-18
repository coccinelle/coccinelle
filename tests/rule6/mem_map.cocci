// should also do the following, but probably not supported
//- #include <linux/wrapper.h>
//- #include "drivers/sound/cs4281/cs4281\_wrapper.h"


// must be first rule!
@@ identifier page; @@
- #define cs4x_mem_map_unreserve(page) mem_map_unreserve(page)

@@ identifier page; @@
- #define cs4x_mem_map_reserve(page) mem_map_reserve(page)

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



// -----------------------------------------------------------------------
// shoud make corrected instead of this
// -----------------------------------------------------------------------
// pourrait meme faire 
// no metavar on params on #define for the moment
//@@ @@
//- #define     MAP_NR(x)       virt_to_page(x)
//
//@@ @@
//- #define     MAP_NR(v)       virt_to_page(v)
//
//@@ expression X; @@ 
//- MAP_NR(X)
//+ virt_to_page(X) 
