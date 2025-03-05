// author: Michele Martone
// description: memory handling functions for allocate.c
#include "tests/ctests/gadget_match.cocci"

@insert_per_type_soa_functions_prototypes@
identifier new_prtcl_str_id.id1;
fresh identifier soa_init_fid="soa_init__"##id1;
fresh identifier soa_alloc_fid="soa_alloc__"##id1;
fresh identifier soa_free_fid="soa_free__"##id1;
@@
++ void soa_init_fid(struct id1*P);
++ void soa_alloc_fid(struct id1*P, size_t N);
++ void soa_free_fid(struct id1*P);
void allocate_memory(...) { ... }

@insert_functions_prototypes@
@@
+ void soa_abort(const char*s);
+ void soa_init(void);
+ void soa_alloc(size_t N);
+ void soa_free(void);
void allocate_memory(...) { ... }

@insert_per_type_soa_functions@
identifier new_prtcl_str_id.id1;
fresh identifier soa_init_fid="soa_init__"##id1;
fresh identifier soa_alloc_fid="soa_alloc__"##id1;
fresh identifier soa_free_fid="soa_free__"##id1;
@@
void allocate_memory(...) { ... }
++ void soa_init_fid(struct id1*P)
++ {
++ }
++ void soa_alloc_fid(struct id1*P, size_t N)
++ {
++ }
++ void soa_free_fid(struct id1*P)
++ {
++ }

@insert_soa_functions@
@@
// declare variables of new struct type
void allocate_memory(...) { ... }
+ void soa_abort(const char*s)
+ {
+ 	// shall print error message here
+ 	abort(); // from stdlib.h
+ }
+
+ void soa_init(void)
+ {
+ }
+
+ void soa_alloc(size_t N)
+ {
+	assert(N>0);
+ }
+
+ void soa_free(void)
+ {
+ }


// Note: can split patch file here.
@fei@
identifier new_prtcl_str_id.id1;
identifier P;
@@
struct id1 P;


@soa_init@
identifier new_prtcl_str_id.id1;
identifier fei.P;
identifier insert_per_type_soa_functions.soa_init_fid;
@@
void soa_init(...)
{ ...
++ soa_init_fid(&P);
}

@soa_free@
identifier new_prtcl_str_id.id1;
identifier fei.P;
identifier insert_per_type_soa_functions.soa_free_fid;
@@
void soa_free(...)
{ ...
++ soa_free_fid(&P);
}

@soa_alloc@
identifier new_prtcl_str_id.id1;
identifier fei.P;
identifier insert_per_type_soa_functions.soa_alloc_fid;
@@
void soa_alloc(...)
{ ...
++ soa_alloc_fid(&P,N);
}

@script:python str_from_id@
M << prtcl_str_mmbrs.M;
str;
@@
// create an identifier and add quotes to it for mymalloc
coccinelle.str="\"%s\""%(M)

@per_type_soa_free@
identifier new_prtcl_str_id.id1;
identifier prtcl_str_mmbrs.M;
symbol P;
identifier insert_per_type_soa_functions.soa_free_fid;
fresh identifier si = "#ifdef "##"HAVE__"##id1##"__"##M##" //";
//identifier soa_alloc.si;
@@
void soa_free_fid(...)
{ ...
++si;
++P->M=NULL;
++	if(P->M) { myfree(P->M); P->M=NULL; }
++#endif
}

@per_type_soa_alloc@
identifier new_prtcl_str_id.id1;
identifier prtcl_str_mmbrs.M;
type prtcl_str_mmbrs.MT;
symbol P;
identifier insert_per_type_soa_functions.soa_alloc_fid;
fresh identifier si = "#ifdef "##"HAVE__"##id1##"__"##M##" //";
//identifier soa_alloc.si;
identifier str_from_id.str;
@@
void soa_alloc_fid(...)
{ ...
++si;
++	P->M = (MT*) mymalloc(str, sizeof(*(P->M)) * N);
++	if(!P->M)soa_abort(/*"allocating "*/ str);
++#endif
}

@per_type_soa_init@
identifier new_prtcl_str_id.id1;
identifier prtcl_str_mmbrs.M;
symbol P;
identifier insert_per_type_soa_functions.soa_init_fid;
fresh identifier si = "#ifdef "##"HAVE__"##id1##"__"##M##" //";
@@
void soa_init_fid(...)
{ ...
++si;
++P->M=NULL;
++#endif
}

@@
@@
void allocate_memory(...) { ...
+	soa_alloc(All.MaxPart);
}

// vim:number:syntax=diff
