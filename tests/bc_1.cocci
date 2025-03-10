@r@
identifier id = {sph_particle_data_buggy};
field list fs;
identifier I;
declaration d;
@@
// identify structure+pointer definitions of specified types
(
struct id { fs } *I;
&
d
)

@script:python s@
id << r.id;
id1;
@@
// construct a specific identifier using python scripting
coccinelle.id1="%s_soa_t"%(id)

@@
identifier s.id1;
field list r.fs;
@@
// insert new data structure definition
++struct id1 { fs };
 struct dens_eval_arg_t { ... };

@@
identifier s.id1;
identifier r.I; // TODO: remember: now on, r.I is just I
fresh identifier J = I ## "_soa";
@@
// declare variables of new struct type
 struct id1 { ... };
++struct id1 J;

// TODO: Following two rules have been provided as a workaround about a coccinelle-sided problem in handling #ifdef..#endif's within anonymous unions definitions.
@f@
identifier s.id1;
identifier J,i;
field list[n] fs;
@@
// match union fields of new struct
struct id1{ fs
 union { ... } J;
  ...
};

@@
identifier s.id1;
field fld;
field list[f.n] fs;
@@
// delete union fields of new struct type definition
struct id1{ fs
- fld
  ...
};
