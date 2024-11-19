// author: Michele Martone
// description:
// This are transitory rules.
// The gather  functions move data from SoA into AoS.
// The scatter functions move data from AoS into SoA.
// After each gather or scatter, the source arrays considered not to be valid anymore, and they are invalidated.
// The invalidate functions are meant mostly as a debug measure.
@@
identifier SFI =~ "(sort|SORT)";
symbol P;
@@
+/* FIXME: this is a transitory gather-sort-scatter hook solution */
+soa_gather__particle_data_soa_t(P, P_soa, 0, NumPart);
+soa_invalidate_soa__particle_data_soa_t(&P_soa, 0, NumPart);
SFI(P,...);
+soa_scatter__particle_data_soa_t(P_soa, P, 0, NumPart);
+soa_invalidate_aos__particle_data(P, 0, NumPart);

@@
@@
+/* FIXME: this is a transitory gather-sort-scatter hook solution */
+soa_gather__sph_particle_data_soa_t(SphP, SphP_soa, 0, N_gas);
+soa_gather__particle_data_soa_t(P, P_soa, 0, NumPart);
+soa_invalidate_soa__sph_particle_data_soa_t(&SphP_soa, 0, N_gas);
+soa_invalidate_soa__particle_data_soa_t(&P_soa, 0, NumPart);
rearrange_particle_sequence(...);
+soa_scatter__particle_data_soa_t(P_soa, P, 0, NumPart);
+soa_scatter__sph_particle_data_soa_t(SphP_soa, SphP, 0, N_gas);
+soa_invalidate_aos__sph_particle_data(SphP, 0, N_gas);
+soa_invalidate_aos__particle_data(P, 0, NumPart);

// applies to GreenTree_AR/recompose_domain.c and domain.c:
@@
@@
+/* FIXME: this is a transitory scatter-modify-gather hook solution */
+/* would be more proper to keep do_box_wrapping() as AoS  */
+soa_scatter__particle_data_soa_t(P_soa, P, 0, NumPart);
 do_box_wrapping();		
+soa_gather__particle_data_soa_t(P, P_soa, 0, NumPart);

// vim:number:syntax=diff
