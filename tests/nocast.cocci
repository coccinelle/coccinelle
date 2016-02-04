@@
identifier f, gfp;
type T;
@@

f(...,
-     T gfp
+     gfp_t gfp
     , ...) {
   ...
   kmalloc(...,gfp)
   ...
}
