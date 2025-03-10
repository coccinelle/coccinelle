#include <stdlib.h>
struct particle_data { int i; double d; } * P;
struct sph_particle_data { int i; double d; } * SphP;

void soa_invalidate_soa__sph_particle_data_soa_t(struct sph_particle_data_soa_t *P_soa,
						 size_t first, size_t N) {
	size_t i;
	if (P_soa == NULL)
		return; //happens. e.g. in runs with gravity only SphP does not exist
	for(i = first;i < first + N;++i) {
	#ifdef HAVE__sph_particle_data_soa_t__d //;
		P_soa->d[i] = NAN;
#endif
	}
}

void soa_invalidate_soa__particle_data_soa_t(struct particle_data_soa_t *P_soa,
					     size_t first, size_t N) {
	size_t i;
	if (P_soa == NULL)
		return; //happens. e.g. in runs with gravity only SphP does not exist
	for(i = first;i < first + N;++i) {
	#ifdef HAVE__particle_data_soa_t__d //;
		P_soa->d[i] = NAN;
#endif
	}
}

void soa_invalidate_aos__sph_particle_data(struct sph_particle_data *P_aos,
					   size_t first, size_t N) {
	size_t i;
	if (P_aos == NULL)
		return; //happens. e.g. in runs with gravity only SphP does not exist
	for(i = first;i < first + N;++i) {
	#ifdef HAVE__sph_particle_data__d //;
		P_aos[i].d = NAN;
#endif
	}
}

void soa_invalidate_aos__particle_data(struct particle_data *P_aos,
				       size_t first, size_t N) {
	size_t i;
	if (P_aos == NULL)
		return; //happens. e.g. in runs with gravity only SphP does not exist
	for(i = first;i < first + N;++i) {
	#ifdef HAVE__particle_data__d //;
		P_aos[i].d = NAN;
#endif
	}
}

//extern int maxThreads;

void allocate_memory() { }

int main()
{
	return 0;
}
