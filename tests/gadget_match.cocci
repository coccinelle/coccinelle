// author: Michele Martone
// description: general matching rules

@prtcl_str@
identifier id = {particle_data,sph_particle_data};
field list fs;
identifier I;
declaration d;
type ST;
@@
// identify structure+pointer definitions of specified types
(
struct id { fs } *I;
&
ST        { fs } *I; 
&
d
)

// usable prtcl_str_mmbrs rule from make_ptr, but slow :-(
@prtcl_str_mmbrs@
identifier prtcl_str.id;
identifier M;
identifier P;
typedef MyDouble;
typedef MyFloat;
typedef MyLongDouble;
typedef MyDoublePos;
typedef MyBigFloat;
typedef MyFloat3;
typedef MyLongDouble3;
typedef MyDoublePos3;
type MT = { double,float,MyDouble,MyFloat,MyLongDouble,MyDoublePos,MyBigFloat,MyLongDouble3,MyFloat3,MyDoublePos3};
@@
// identify structure+pointer definitions of specified types
struct id { ...
MT M;
... } *P;

@script:python new_prtcl_str_id@
id << prtcl_str.id;
id1;
@@
// construct a specific identifier using python scripting
coccinelle.id1="%s_soa_t"%(id)

@new_prtcl_str@
identifier new_prtcl_str_id.id1;
field list[n] fs;
position p;
type T;
identifier I;
@@
// match position of new struct field declarations
struct id1{ fs
 T I@p;
  ...
};

@script:python new_prtcl_str_var_id@
I << prtcl_str.I;
J; // aka fresh identifier J = I ## "_soa";
@@
// construct a specific identifier using python scripting
coccinelle.J="%s_soa"%(I)

// vim:number:syntax=diff
