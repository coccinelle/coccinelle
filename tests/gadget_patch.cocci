// author: Michele Martone
// description: general AoS -> SoA expressions patch

@ostr@
identifier id = {particle_data,sph_particle_data};
identifier P;
@@
// match original particle structure variable declarations
struct id{ ... } *P;

@script:python pps@
id << ostr.id;
id1;
@@
// create soa struct type identifier using python scripting
coccinelle.id1="%s_soa_t"%(id)

@nt@
identifier pps.id1;
identifier I;
type T;
@@
// match fields of id1
struct  id1 {
 ...
 T I;
 ...
};

@script:python pid@
id1 << pps.id1;
P << ostr.P;
S;
@@
// append identifier _soa to P
coccinelle.S="%s_soa"%(P)

@n@
identifier pid.S;
identifier pps.id1;
@@
// match soa variable instance
struct  id1 S;

@soa_access@
identifier ostr.P;
identifier pid.S;
identifier nt.I;
expression E;
@@
// AoS  -> SoA access
-P[E].I
+S.I[E]

// vim:number:syntax=diff
