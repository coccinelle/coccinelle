@@
struct agp_bridge_driver s;
!function f;
@@

  <...
  s.cache_flush = f;
  ...>
  ooo
  <...
- CACHE_FLUSH()
+ f
  ...>

@@
identifier agp_bridge;
@@

  struct agp_bridge_driver agp_bridge;
  ...
- CACHE_FLUSH()
+ agp_bridge->driver->cache_flush
@@
local function fn;
identifier agp_bridge;
@@

  struct agp_bridge_driver agp_bridge;
  <...
  fn(...) {
    <...
-   CACHE_FLUSH()
+   agp_bridge->driver->cache_flush
    ...>
  ...>
