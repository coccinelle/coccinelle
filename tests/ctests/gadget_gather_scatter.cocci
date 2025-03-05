// author: Michele Martone
// description: gather/scatter

// Part 1: applies only to allocate.c
#include "tests/ctests/gadget_match.cocci"
@gather_per_type_soa_functions@
identifier new_prtcl_str_id.id1;
identifier prtcl_str.id;
fresh identifier soa_gather_fid="soa_gather__"##id1;
symbol first;
@@
++ void soa_gather_fid(struct id*P_aos, const struct id1 P_soa, size_t first, size_t N)
++ {
++   size_t i;
++   if(P_aos==NULL)  return; //happens. e.g. in runs with gravity only SphP does not exist
++   for(i=first;i<first+N;++i) {}
++ }
void allocate_memory(...) { ... }

@gather_per_type_soa_functions_fill@
identifier new_prtcl_str_id.id1;
identifier prtcl_str_mmbrs.M;
identifier prtcl_str.id;
identifier P_aos, P_soa, N;
identifier gather_per_type_soa_functions.soa_gather_fid;
fresh identifier si = "#ifdef "##"HAVE__"##id1##"__"##M##" //";
symbol first;
@@
void soa_gather_fid(struct id*P_aos, const struct id1 P_soa, size_t first, size_t N)
{ ... for (...;...;...) { ...
++si;
++//P_aos[i].M = P_soa.M[i];
++memcpy(&P_aos[i].M,&P_soa.M[i],sizeof(P_aos[i].M));//Note:temporary, shall be restricted to triplets only
++#endif
}
}

@scatter_per_type_soa_functions@
identifier new_prtcl_str_id.id1;
identifier prtcl_str.id;
fresh identifier soa_scatter_fid="soa_scatter__"##id1;
symbol first;
@@
++ void soa_scatter_fid(struct id1 P_soa, const struct id*P_aos, size_t first, size_t N)
++ {
++ size_t i;
++   if(P_aos==NULL)  return; //happens. e.g. in runs with gravity only SphP does not exist
++   for(i=first;i<first+N;++i) {}
++ }
void allocate_memory(...) { ... }

@scatter_per_type_soa_functions_fill@
identifier new_prtcl_str_id.id1;
identifier prtcl_str_mmbrs.M;
identifier prtcl_str.id;
identifier P_aos, P_soa, N;
identifier scatter_per_type_soa_functions.soa_scatter_fid;
fresh identifier si = "#ifdef "##"HAVE__"##id1##"__"##M##" //";
@@
void soa_scatter_fid(struct id1 P_soa, const struct id*P_aos, size_t first, size_t N)
{ ... for (...;...;...) { ...
++si;
++//P_soa.M[i] = P_aos[i].M;
++memcpy(&P_soa.M[i],&P_aos[i].M,sizeof(P_aos[i].M));//Note:temporary, shall be restricted to triplets only
++#endif
}
}

// Part 2: applies only to domain.c
@call_gather_per_type_soa_functions@
identifier new_prtcl_str_id.id1;
fresh identifier soa_gather_fid="soa_gather__"##id1;
identifier F = domain_Decomposition;
identifier prtcl_str.I;
identifier new_prtcl_str_var_id.J;
//statement S1, S2;
// Note: avoid scather/gather for DomainBuf*
symbol P;
@@
F (...) {
//... when != S1
//++ soa_gather_fid(I,J,0,sizeof(*I)==sizeof(*P)?NumPart:N_gas /*mega trick because P and SphP arrays are allocated with a different size. */ /*Note: if you see this line in the code, something weng wrong with the semantic patch */);
//S2
//... when any
  ...
++ soa_gather_fid(I,J,0,sizeof(*I)==sizeof(*P)?NumPart:N_gas /*mega trick because P and SphP arrays are allocated with a different size. */ /*Note: if you see this line in the code, something weng wrong with the semantic patch */);
  force_treefree();
  ...
}

@correct_call_gather_per_type_soa_functions_P@
identifier soa_gather_fid =~ "soa_gather__";
identifier I={P,DomainPartBuf};
identifier J;
expression S;
expression E;
@@
- soa_gather_fid(I,J,S,E);
+ soa_gather_fid(I,J,S,NumPart);

@correct_call_gather_per_type_soa_functions_SphP@
identifier soa_gather_fid =~ "soa_gather__";
identifier I={SphP,DomainSphBuf};
identifier J;
expression S;
expression E;
@@
- soa_gather_fid(I,J,S,E);
+ soa_gather_fid(I,J,S,N_gas);

@call_scatter_per_type_soa_functions@
identifier new_prtcl_str_id.id1;
fresh identifier soa_scatter_fid="soa_scatter__"##id1;
identifier F = domain_Decomposition;
identifier prtcl_str.I;
identifier new_prtcl_str_var_id.J;
// Note: adding gather before } slows down extremely
//       (why is so slow ? brings from a few to ~30s...)
symbol P;
@@
F (...) {
...
++ soa_scatter_fid(J,I,0,sizeof(*I)==sizeof(*P)?NumPart:N_gas /*mega trick because P and SphP arrays are allocated with a different size. */);
}

@correct_call_scatter_per_type_soa_functions_P@
identifier soa_scatter_fid =~ "soa_scatter__";
identifier I={P_soa,DomainPartBuf_soa};
identifier J;
expression S;
expression E;
@@
- soa_scatter_fid(I,J,S,E);
+ soa_scatter_fid(I,J,S,NumPart);

@correct_call_scatter_per_type_soa_functions_SphP@
identifier soa_scatter_fid =~ "soa_scatter__";
identifier I={SphP_soa,DomainSphBuf_soa};
identifier J;
expression S;
expression E;
@@
- soa_scatter_fid(I,J,S,E);
+ soa_scatter_fid(I,J,S,N_gas);

// Part 3: applies only to allvars.h
@decl_gather_scatter_functions@
identifier new_prtcl_str_id.id1;
fresh identifier soa_gather_fid="soa_gather__"##id1;
fresh identifier soa_scatter_fid="soa_scatter__"##id1;
identifier prtcl_str.id;
symbol maxThreads;
symbol first;
@@
extern int maxThreads;
++ void soa_gather_fid(struct id*P_aos, const struct id1 P_soa, size_t first, size_t N);
++ void soa_scatter_fid(struct id1 P_soa, const struct id*P_aos, size_t first, size_t N);

// Part 4: applies only to forcetree.c
@@
symbol pa,p,P;
@@
-pa = &P[p];
+pa = &P[p]; soa_gather__particle_data_soa_t(pa-p,P_soa,p,1)/* Note: patched temporarily */;

// vim:number:syntax=diff
