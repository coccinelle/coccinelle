// author: Michele Martone
// description: introduce typedef for 3x arrays
// triplets rules BEGIN

@triplets_typedef_inject@
@@
+typedef MyLongDouble MyLongDouble3[3];
+typedef MyFloat MyFloat3[3];
enum cpufields {...}; // ok for OpenGadget3, IPCC-Gadget
//struct unbind_data {...}; // ok for OpenGadget3

@triplets_typedef_retype_MyLongDouble3@
typedef MyLongDouble3;
identifier I;
identifier id = {particle_data,sph_particle_data};
field list[n] fs;
@@
struct id { fs
(
MyLongDouble I[...][...]; // matches all arrays dim 2+
|
-MyLongDouble I[3];
+MyLongDouble3 I;
)
  ...
}

@triplets_typedef_retype_MyFloat3@
typedef MyFloat3;
identifier I;
identifier id = {particle_data,sph_particle_data};
field list[n] fs;
@@
struct id { fs
(
MyFloat I[...][...]; // matches all arrays dim 2+
|
-MyFloat I[3];
+MyFloat3 I;
)
  ...
}

// triplets rules END
// vim:number:syntax=diff
