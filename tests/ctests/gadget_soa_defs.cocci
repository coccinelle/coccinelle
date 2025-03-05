// author: Michele Martone
// description: patches allvars.h header
#include "tests/ctests/gadget_match.cocci"

// Note: requires absence of ALIGN(32) occurrences!

@insert_new_prtcl_str depends on prtcl_str@
identifier new_prtcl_str_id.id1;
field list prtcl_str.fs;
symbol maxThreads;
@@
// insert new data structure definition
//extern int maxThreads; // OpenGadget3
++struct id1 { fs };
extern int maxThreads; // OpenGadget3, IPCC

@insert_new_prtcl_str_var_extr@
identifier new_prtcl_str_id.id1;
identifier prtcl_str.I;
fresh identifier J = I ## "_soa";
@@
struct id1 { ... };
// declare variables of new struct type
++extern struct id1 J;

// can I put this in a file and include it multiple times ?
@match_anon_union_in_struct@
identifier new_prtcl_str_id.id1;
identifier J;
field list[n] fs;
@@
// match union fields of new struct
struct id1{ fs
 union { ... } J;
  ...
};

@rm_union_from_struct depends on match_anon_union_in_struct@
field list[match_anon_union_in_struct.n] fs;
identifier new_prtcl_str_id.id1;
field fld;
@@
// delete union fields of new struct type definition
struct id1{ fs
- fld
  ...
};

@make_ptr@
identifier new_prtcl_str_id.id1;
identifier M;
typedef MyDouble;
typedef MyFloat;
typedef MyLongDouble;
typedef MyDoublePos;
typedef MyBigFloat;
typedef MyFloat3;
typedef MyLongDouble3;
typedef MyDoublePos3;
type MT = { double,float,MyDouble,MyFloat,MyLongDouble,MyDoublePos,MyBigFloat,MyLongDouble3,MyFloat3,MyDoublePos3};
//identifier prtcl_str_mmbrs.M;//slow
//type prtcl_str_mmbrs.MT;//slow
@@
// make pointers out soa struct fields (see types whitelist above)
struct id1{ ...
- MT M;
++ MT*M;
  ...
};

@del_non_ptr@
identifier new_prtcl_str_id.id1;
identifier J;
type T;
type P != {T*};
@@
// delete any field which is not a pointer (=which has not been transformed into a pointer)
struct id1{ ...
- P J;
  ...
};

// vim:number:syntax=diff
