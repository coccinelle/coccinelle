// author: Michele Martone
// description: introduce typedef for 3x3 arrays
// ninelets rules BEGIN

@ninelets_typedef_inject@
@@
+typedef double double3x3[3][3];
+typedef MyFloat MyFloat3x3[3][3];
enum cpufields {...}; // ok for OpenGadget3, IPCC-Gadget
//struct unbind_data {...}; // ok for OpenGadget3

@ninelets_typedef_retype_MyFloat3x3@
typedef MyFloat3x3;
identifier I;
identifier id = {particle_data,sph_particle_data};
field list[n] fs;
@@
struct id { fs
-MyFloat I[3][3];
+MyFloat3x3 I;
  ...
}

@ninelets_typedef_retype_double3x3@
typedef double3x3;
identifier I;
identifier id = {particle_data,sph_particle_data};
field list[n] fs;
@@
struct id { fs
-double I[3][3];
+double3x3 I;
  ...
}

// ninelets rules END
// vim:number:syntax=diff
