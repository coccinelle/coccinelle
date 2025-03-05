// author: Michele Martone
// description: (still incomplete) debug / sentinel code patch

#include "tests/ctests/gadget_match.cocci"

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

@checksum_per_type_soa_functions@
identifier new_prtcl_str_id.id1;
fresh identifier soa_checksum_fid="soa_checksum__"##id1;
@@
++ long double soa_checksum_fid(const struct id1*P_soa, size_t first, size_t N)
++ {
++   size_t i;
++   long double cksumval = 0;
++   if(P_soa==NULL)  return cksumval; //happens. e.g. in runs with gravity only SphP does not exist
++   for(i=first;i<first+N;++i) {}
++   return cksumval;
++ }
void allocate_memory(...) { ... }

@checksum_per_type_soa_functions_compute@
identifier new_prtcl_str_id.id1;
identifier prtcl_str_mmbrs_scalars.M;
identifier P_soa, N;
identifier checksum_per_type_soa_functions.soa_checksum_fid;
fresh identifier si = "#ifdef "##"HAVE__"##id1##"__"##M##" //";
@@
long double soa_checksum_fid(const struct id1*P_soa, size_t first, size_t N)
{ ... for (...;...;...) { ...
++si;
++cksumval += P_soa->M[i];
++#endif
}
...
}

@checksum_per_type_aos_functions@
identifier prtcl_str.id;
fresh identifier aos_checksum_fid="aos_checksum__"##id;
@@
++ long double aos_checksum_fid(const struct id*P_aos, size_t first, size_t N)
++ {
++   size_t i;
++   long double cksumval = 0;
++   if(P_aos==NULL)  return cksumval; //happens. e.g. in runs with gravity only SphP does not exist
++   for(i=first;i<first+N;++i) {}
++   return cksumval;
++ }
void allocate_memory(...) { ... }

@checksum_per_type_aos_functions_compute@
identifier new_prtcl_str_id.id1;
identifier prtcl_str_mmbrs_scalars.M;
identifier prtcl_str.id;
identifier P_aos, N;
identifier checksum_per_type_aos_functions.aos_checksum_fid;
fresh identifier si = "#ifdef "##"HAVE__"##id1##"__"##M##" //";
@@
long double aos_checksum_fid(const struct id*P_aos, size_t first, size_t N)
{ ... for (...;...;...) { ...
++si;
++cksumval += P_aos[i].M;
++#endif
}
...
}

@@
identifier F != {main,soa_checksum__sph_particle_data_soa_t ,soa_checksum__particle_data_soa_t ,aos_checksum__sph_particle_data ,aos_checksum__particle_data};
statement S1,S2;
type T;
@@
T F (...) 
{
 ... when != S1
+{char*gds = getenv("GADGET_DEBUG");
+if(gds)
+{
+if(strchr(gds,'?'))
+  printf("p for AoS P\n"),
+  printf("P for SoA P\n"),
+  printf("s for AoS SphP\n"),
+  printf("S for Soa SphP\n");
+if((!*gds) || strchr(gds,'S'))
+  printf("in %s(): S: %llg\n",__FUNCTION__,soa_checksum__sph_particle_data_soa_t(&SphP_soa,0, N_gas  ));
+if((!*gds) || strchr(gds,'P'))
+  printf("in %s(): P: %llg\n",__FUNCTION__,soa_checksum__particle_data_soa_t    (&P_soa,   0, NumPart));
+if((!*gds) || strchr(gds,'s'))
+  printf("in %s(): s: %llg\n",__FUNCTION__,aos_checksum__sph_particle_data      ( SphP,    0, N_gas  ));
+if((!*gds) || strchr(gds,'p'))
+  printf("in %s(): p: %llg\n",__FUNCTION__,aos_checksum__particle_data          ( P,       0, NumPart));
+}}
 S2
 ... when any
}

// vim:number:syntax=diff
