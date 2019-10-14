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

void allocate_memory() { return; }
void force_treefree(){}

void domain_Decomposition()
{
	// declarations:
	int i;

	// statements:
	i=0;

  	force_treefree();
	/* */

	// end of function
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
	return 0;
}
