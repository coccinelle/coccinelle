// author: Michele Martone
// description:
// triplets rules BEGIN
// IPCC version specific

@triplets_typedef_inject@
@@
+typedef MyDoublePos MyDoublePos3[3];
enum cpufields {...}; // ok for OpenGadget3, IPCC-Gadget

@triplets_typedef_retype_MyDoublePos3@
typedef MyDoublePos3;
identifier I;
identifier id = {particle_data,sph_particle_data};
field list[n] fs;
@@
struct id { fs
(
MyDoublePos I[...][...]; // matches all arrays dim 2+
|
-MyDoublePos I[3];
+MyDoublePos3 I;
)
  ...
}

// triplets rules END
// vim:number:syntax=diff
