// author: Michele Martone
// description:
// This is a debug rule.
// It marks identifiers from the old data structures, which are already in the new one.

@prtcl_str@
identifier id = {particle_data, sph_particle_data};
@@
struct id {
 ...
}

@script:python new_prtcl_str_id@
id << prtcl_str.id;
id1;
@@
// construct a specific identifier using python scripting
coccinelle.id1="%s_soa_t"%(id)

@p_new_str@
type T;
identifier I;
identifier new_prtcl_str_id.id1;
@@
struct id1 
{
...
 T I;
...
}

@p_rm_str@
identifier prtcl_str.id;
type p_new_str.T;
identifier p_new_str.I;
@@
struct id {
...
-T I;
+T I; /* also in SoA now */
...
}

// vim:number:syntax=diff
