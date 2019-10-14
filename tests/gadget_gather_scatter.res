#include <stdlib.h>
#include <stdio.h>
int NumPart;
int N_gas;

#define HAVE__sph_particle_data_soa_t__d 1
#define HAVE__particle_data_soa_t__d 1
#define HAVE__sph_particle_data__d 1
#define HAVE__particle_data__d 1

struct particle_data { int i; double d; } * P, *DomainPartBuf;
struct sph_particle_data { int i; double d; } * SphP;

struct particle_data_soa_t { int*i; double*d; } P_soa, DomainPartBuf_soa;
struct sph_particle_data_soa_t { int*i; double*d; } SphP_soa;

extern int maxThreads;
void soa_gather__sph_particle_data_soa_t(struct sph_particle_data *P_aos,
					 const struct sph_particle_data_soa_t P_soa,
					 size_t first, size_t N);
void soa_scatter__sph_particle_data_soa_t(struct sph_particle_data_soa_t P_soa,
					  const struct sph_particle_data *P_aos,
					  size_t first, size_t N);
void soa_gather__particle_data_soa_t(struct particle_data *P_aos,
				     const struct particle_data_soa_t P_soa,
				     size_t first, size_t N);
void soa_scatter__particle_data_soa_t(struct particle_data_soa_t P_soa,
				      const struct particle_data *P_aos,
				      size_t first, size_t N);

void soa_gather__sph_particle_data_soa_t(struct sph_particle_data *P_aos,
					 const struct sph_particle_data_soa_t P_soa,
					 size_t first, size_t N) {
	size_t i;
	if (P_aos == NULL)
		return; //happens. e.g. in runs with gravity only SphP does not exist
	for(i = first;i < first + N;++i) {
	#ifdef HAVE__sph_particle_data_soa_t__d //;
		//P_aos[i].M = P_soa.M[i];
		memcpy(&P_aos[i].d, &P_soa.d[i], sizeof(P_aos[i].d));//FIXME:temporary, shall be restricted to triplets only
#endif
	}
}

void soa_gather__particle_data_soa_t(struct particle_data *P_aos,
				     const struct particle_data_soa_t P_soa,
				     size_t first, size_t N) {
	size_t i;
	if (P_aos == NULL)
		return; //happens. e.g. in runs with gravity only SphP does not exist
	for(i = first;i < first + N;++i) {
	#ifdef HAVE__particle_data_soa_t__d //;
		//P_aos[i].M = P_soa.M[i];
		memcpy(&P_aos[i].d, &P_soa.d[i], sizeof(P_aos[i].d));//FIXME:temporary, shall be restricted to triplets only
#endif
	}
}

void soa_scatter__sph_particle_data_soa_t(struct sph_particle_data_soa_t P_soa,
					  const struct sph_particle_data *P_aos,
					  size_t first, size_t N) {
	size_t i;
	if (P_aos == NULL)
		return; //happens. e.g. in runs with gravity only SphP does not exist
	for(i = first;i < first + N;++i) {
	#ifdef HAVE__sph_particle_data_soa_t__d //;
		//P_soa.M[i] = P_aos[i].M;
		memcpy(&P_soa.d[i], &P_aos[i].d, sizeof(P_aos[i].d));//FIXME:temporary, shall be restricted to triplets only
#endif
	}
}

void soa_scatter__particle_data_soa_t(struct particle_data_soa_t P_soa,
				      const struct particle_data *P_aos,
				      size_t first, size_t N) {
	size_t i;
	if (P_aos == NULL)
		return; //happens. e.g. in runs with gravity only SphP does not exist
	for(i = first;i < first + N;++i) {
	#ifdef HAVE__particle_data_soa_t__d //;
		//P_soa.M[i] = P_aos[i].M;
		memcpy(&P_soa.d[i], &P_aos[i].d, sizeof(P_aos[i].d));//FIXME:temporary, shall be restricted to triplets only
#endif
	}
}

void allocate_memory() { return; }
void force_treefree(){}

void domain_Decomposition()
{
	// declarations:
	int i;

	// statements:
	i=0;

  	soa_gather__sph_particle_data_soa_t(SphP, SphP_soa, 0, N_gas);
  	soa_gather__particle_data_soa_t(P, P_soa, 0, NumPart);
  	soa_gather__particle_data_soa_t(DomainPartBuf, DomainPartBuf_soa, 0,
  	  	  	  	  NumPart);
  	force_treefree();
	/* */

	// end of function
  	soa_scatter__sph_particle_data_soa_t(SphP_soa, SphP, 0, N_gas);
  	soa_scatter__particle_data_soa_t(P_soa, P, 0, NumPart);
  	soa_scatter__particle_data_soa_t(DomainPartBuf_soa, DomainPartBuf,
  	  	  	  	   0, NumPart);
}

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

	DomainPartBuf_soa.d = malloc(sizeof(*DomainPartBuf_soa.d)*NumPart) ;
	for(i = 0;i < NumPart;++i)
	  DomainPartBuf_soa.d[i]=i+1;

	DomainPartBuf = malloc(sizeof(*DomainPartBuf)*NumPart) ;
	for(i = 0;i < NumPart;++i)
	  DomainPartBuf[i].d=i+1;



	SphP_soa.d = malloc(sizeof(*SphP_soa.d)*N_gas) ;
	for(i = 0;i < N_gas;++i)
	  SphP_soa.d[i]=i+1;

	SphP = malloc(sizeof(*SphP)*N_gas) ;
	for(i = 0;i < N_gas;++i)
	  SphP[i].d=i+1;

	allocate_memory();

	domain_Decomposition();
 	
	struct particle_data *pa;
	int p =0;
	pa = &P[p];
	soa_gather__particle_data_soa_t(pa - p, P_soa, p, 1)/* FIXME: patched temporarily */;
	return 0;
}
