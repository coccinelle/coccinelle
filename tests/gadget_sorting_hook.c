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
	any_sort_func(P);


	rearrange_particle_sequence();

  	return 0;
}
