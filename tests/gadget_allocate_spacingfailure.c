#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
int NumPart;
int N_gas;

#define HAVE__sph_particle_data_soa_t__d 1
#define HAVE__particle_data_soa_t__d 1
#define HAVE__sph_particle_data__d 1
#define HAVE__particle_data__d 1

extern int maxThreads;
void * myfree(void*p){return NULL;}
void * mymalloc(){return NULL;}
union {int MaxPart;} All;
struct particle_data { int i; double d; } * P, *DomainPartBuf;
struct sph_particle_data { int i; double d; } * SphP;

struct particle_data_soa_t { int*i; double*d; };
struct particle_data_soa_t P_soa, DomainPartBuf_soa;
struct sph_particle_data_soa_t { int*i; double*d; };
struct sph_particle_data_soa_t SphP_soa;

void allocate_memory() { return; }

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

	return 0;
}
