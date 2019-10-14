#include <stdlib.h>
void *SphP, *SphP_soa;
void *P_soa;
int N_gas, NumPart;

struct s { int i; } * P; 
void any_sort_func(struct s*P){}
void rearrange_particle_sequence(){}

void soa_gather__sph_particle_data_soa_t(){}
void soa_gather__particle_data_soa_t(){}
void soa_invalidate_soa__sph_particle_data_soa_t(){}
void soa_invalidate_soa__particle_data_soa_t(){}
void soa_scatter__particle_data_soa_t(){}
void soa_scatter__sph_particle_data_soa_t(){}
void soa_invalidate_aos__sph_particle_data(){}
void soa_invalidate_aos__particle_data(){}

int main()
{
	/* FIXME: this is a transitory gather-sort-scatter hook solution */
	soa_gather__particle_data_soa_t(P, P_soa, 0, NumPart);
	soa_invalidate_soa__particle_data_soa_t(&P_soa, 0, NumPart);
	any_sort_func(P);
	soa_scatter__particle_data_soa_t(P_soa, P, 0, NumPart);
	soa_invalidate_aos__particle_data(P, 0, NumPart);


	/* FIXME: this is a transitory gather-sort-scatter hook solution */
	soa_gather__sph_particle_data_soa_t(SphP, SphP_soa, 0, N_gas);
	soa_gather__particle_data_soa_t(P, P_soa, 0, NumPart);
	soa_invalidate_soa__sph_particle_data_soa_t(&SphP_soa, 0, N_gas);
	soa_invalidate_soa__particle_data_soa_t(&P_soa, 0, NumPart);
	rearrange_particle_sequence();
  	soa_scatter__particle_data_soa_t(P_soa, P, 0, NumPart);
  	soa_scatter__sph_particle_data_soa_t(SphP_soa, SphP, 0, N_gas);
  	soa_invalidate_aos__sph_particle_data(SphP, 0, N_gas);
  	soa_invalidate_aos__particle_data(P, 0, NumPart);

  	return 0;
}
