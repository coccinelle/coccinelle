#include <stdlib.h>
#include <stdio.h>
int NumPart;
int N_gas;

#define HAVE__sph_particle_data_soa_t__d 1
#define HAVE__particle_data_soa_t__d 1
#define HAVE__sph_particle_data__d 1
#define HAVE__particle_data__d 1

struct particle_data { int i; double d; } * P;
struct sph_particle_data { int i; double d; } * SphP;

struct particle_data_soa_t { int*i; double*d; } P_soa;
struct sph_particle_data_soa_t { int*i; double*d; } SphP_soa;

long double soa_checksum__sph_particle_data_soa_t(const struct sph_particle_data_soa_t *P_soa,
						  size_t first, size_t N) {
	size_t i;
	long double cksumval = 0;
	if (P_soa == NULL)
		return cksumval; //happens. e.g. in runs with gravity only SphP does not exist
	for(i = first;i < first + N;++i) {
	#ifdef HAVE__sph_particle_data_soa_t__d //;
		cksumval += P_soa->d[i];
#endif
	}
	return cksumval;
}

long double soa_checksum__particle_data_soa_t(const struct particle_data_soa_t *P_soa,
					      size_t first, size_t N) {
	size_t i;
	long double cksumval = 0;
	if (P_soa == NULL)
		return cksumval; //happens. e.g. in runs with gravity only SphP does not exist
	for(i = first;i < first + N;++i) {
	#ifdef HAVE__particle_data_soa_t__d //;
		cksumval += P_soa->d[i];
#endif
	}
	return cksumval;
}

long double aos_checksum__sph_particle_data(const struct sph_particle_data *P_aos,
					    size_t first, size_t N) {
	size_t i;
	long double cksumval = 0;
	if (P_aos == NULL)
		return cksumval; //happens. e.g. in runs with gravity only SphP does not exist
	for(i = first;i < first + N;++i) {
	#ifdef HAVE__sph_particle_data_soa_t__d //;
		cksumval += P_aos[i].d;
#endif
	}
	return cksumval;
}

long double aos_checksum__particle_data(const struct particle_data *P_aos,
					size_t first, size_t N) {
	size_t i;
	long double cksumval = 0;
	if (P_aos == NULL)
		return cksumval; //happens. e.g. in runs with gravity only SphP does not exist
	for(i = first;i < first + N;++i) {
	#ifdef HAVE__particle_data_soa_t__d //;
		cksumval += P_aos[i].d;
#endif
	}
	return cksumval;
}

void allocate_memory() { {
		char *gds = getenv("GADGET_DEBUG");
		if (gds) {
			if (strchr(gds, '?'))
				printf("p for AoS P\n"), printf("P for SoA P\n"), printf("s for AoS SphP\n"), printf("S for Soa SphP\n");
			if ((!*gds) || strchr(gds, 'S'))
				printf("in %s(): S: %llg\n", __FUNCTION__,
				       soa_checksum__sph_particle_data_soa_t(&SphP_soa, 0, N_gas));
			if ((!*gds) || strchr(gds, 'P'))
				printf("in %s(): P: %llg\n", __FUNCTION__,
				       soa_checksum__particle_data_soa_t(&P_soa, 0, NumPart));
			if ((!*gds) || strchr(gds, 's'))
				printf("in %s(): s: %llg\n", __FUNCTION__,
				       aos_checksum__sph_particle_data(SphP, 0, N_gas));
			if ((!*gds) || strchr(gds, 'p'))
				printf("in %s(): p: %llg\n", __FUNCTION__,
				       aos_checksum__particle_data(P, 0, NumPart));
		}
	}
	return; }

int main()
{
	int i;
	NumPart=3;
	N_gas=2;

	P_soa.d = malloc(sizeof(*P_soa.d)*NumPart) ;
	for(i = 0;i < NumPart;++i)
	  P_soa.d[i]=i+1;

	P = malloc(sizeof(*P)*NumPart) ;
	for(i = 0;i < NumPart;++i)
	  P[i].d=i+1;

	SphP_soa.d = malloc(sizeof(*SphP_soa.d)*N_gas) ;
	for(i = 0;i < N_gas;++i)
	  SphP_soa.d[i]=i+1;

	SphP = malloc(sizeof(*SphP)*N_gas) ;
	for(i = 0;i < N_gas;++i)
	  SphP[i].d=i+1;

	allocate_memory();
	return 0;
}
