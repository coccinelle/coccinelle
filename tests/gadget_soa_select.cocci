// author: Michele Martone
// description:
// This is a sample patch for selective removal of fields.
// It is meant to be applied AFTER header SoA transformation, and BEFORE non-header SoA transformations.
// You can customize it to your likings.
@@
type T;
//identifier I != {keep_this,keep_that}; // 'keep' list
identifier I  = {Entropy}; // 'erase' list
@@
struct sph_particle_data_soa_t {
...
-T*I;
+T*I; /* See coccinelle/gadget_soa_select.cocci on how to select SoA fields on a name basis */
...
};

@keep_pos_as_only_MyDoublePos3@
identifier I != {Pos}; // 'keep' list
@@
struct particle_data_soa_t {
...
-MyDoublePos3*I;
...
};

// vim:number:syntax=diff
