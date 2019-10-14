// author: Michele Martone
// description:
// This is a debug rule.
// It removes identifiers from the old data structures, which are already in the new one.

@p_old_str@
type T;
identifier I;
@@
struct particle_data_soa_t {
...
 T I;
...
}

@p_new_str@
type T;
identifier p_old_str.I;
@@
struct particle_data {
...
-T I;
...
}



@s_old_str@
type T;
identifier I;
@@
struct sph_particle_data_soa_t {
...
 T I;
...
}

@s_new_str@
type T;
identifier s_old_str.I;
@@
struct sph_particle_data {
...
-T I;
...
}

// vim:number:syntax=diff
