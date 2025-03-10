// author: Michele Martone
// description: invalidate_soa/invalidate_aos
#include "tests/gadget_match.cocci"

// usable prtcl_str_mmbrs_scalars rule from make_ptr, but slow :-(
@prtcl_str_mmbrs_scalars@
identifier prtcl_str.id;
identifier M;
identifier P;
typedef MyDouble;
typedef MyFloat;
typedef MyLongDouble;
typedef MyDoublePos;
typedef MyBigFloat;
type MT = { double,float,MyDouble,MyFloat,MyLongDouble,MyDoublePos,MyBigFloat };
@@
// identify structure+pointer definitions of specified types
struct id { ...
MT M;
... } *P;

// Part 1: applies only to allocate.c
@invalidate_soa_per_type_soa_functions@
identifier new_prtcl_str_id.id1;
fresh identifier soa_invalidate_soa_fid="soa_invalidate_soa__"##id1;
@@
++ void soa_invalidate_soa_fid(struct id1*P_soa, size_t first, size_t N)
++ {
++   size_t i;
++   if(P_soa==NULL)  return; //happens. e.g. in runs with gravity only SphP does not exist
++   for(i=first;i<first+N;++i) {}
++ }
void allocate_memory(...) { ... }

@invalidate_soa_per_type_soa_functions_fill@
identifier new_prtcl_str_id.id1;
identifier prtcl_str_mmbrs_scalars.M;
identifier invalidate_soa_per_type_soa_functions.soa_invalidate_soa_fid;
fresh identifier si = "#ifdef "##"HAVE__"##id1##"__"##M##" //";
identifier P_soa, first, N;
@@
void soa_invalidate_soa_fid(struct id1*P_soa, size_t first, size_t N)
{ ... for (...;...;...) { ...
++si;
++P_soa->M[i] = NAN;
++#endif
}
}

@invalidate_aos_per_type_soa_functions@
identifier prtcl_str.id;
fresh identifier soa_invalidate_aos_fid="soa_invalidate_aos__"##id;
@@
++ void soa_invalidate_aos_fid(struct id*P_aos, size_t first, size_t N)
++ {
++   size_t i;
++   if(P_aos==NULL)  return; //happens. e.g. in runs with gravity only SphP does not exist
++   for(i=first;i<first+N;++i) {}
++ }
void allocate_memory(...) { ... }

@invalidate_aos_per_type_soa_functions_fill@
identifier prtcl_str_mmbrs_scalars.M;
identifier prtcl_str.id;
identifier P_aos, first, N;
identifier invalidate_aos_per_type_soa_functions.soa_invalidate_aos_fid;
fresh identifier si = "#ifdef "##"HAVE__"##id##"__"##M##" //";
@@
void soa_invalidate_aos_fid(struct id*P_aos, size_t first, size_t N)
{ ... for (...;...;...) { ...
++si;
++P_aos[i].M = NAN;
++#endif
}
}

// Part 3: applies only to allvars.h
@decl_invalidate_soa_invalidate_aos_functions@
identifier new_prtcl_str_id.id1;
identifier prtcl_str.id;
fresh identifier soa_invalidate_soa_fid="soa_invalidate_soa__"##id1;
fresh identifier soa_invalidate_aos_fid="soa_invalidate_aos__"##id;
symbol maxThreads;
@@
extern int maxThreads;
++ void soa_invalidate_soa_fid(struct id1*P_soa, size_t first, size_t N);
++ void soa_invalidate_aos_fid(struct id *P_aos, size_t first, size_t N);

// vim:number:syntax=diff
