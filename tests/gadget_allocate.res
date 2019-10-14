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

void soa_init__sph_particle_data_soa_t(struct sph_particle_data_soa_t *P);
void soa_alloc__sph_particle_data_soa_t(struct sph_particle_data_soa_t *P,
					size_t N);
void soa_free__sph_particle_data_soa_t(struct sph_particle_data_soa_t *P);
void soa_init__particle_data_soa_t(struct particle_data_soa_t *P);
void soa_alloc__particle_data_soa_t(struct particle_data_soa_t *P, size_t N);
void soa_free__particle_data_soa_t(struct particle_data_soa_t *P);
void soa_abort(const char *s);
void soa_init(void);
void soa_alloc(size_t N);
void soa_free(void);
void allocate_memory() { return; soa_alloc(All.MaxPart);
}

void soa_abort(const char *s) {
	// shall print error message here
	abort(); // from stdlib.h
}
void soa_init(void) {
	soa_init__sph_particle_data_soa_t(&SphP_soa);
	soa_init__particle_data_soa_t(&P_soa);
	soa_init__particle_data_soa_t(&DomainPartBuf_soa);
}
void soa_alloc(size_t N) {
	assert(N > 0);
	soa_alloc__sph_particle_data_soa_t(&SphP_soa, N);
	soa_alloc__particle_data_soa_t(&P_soa, N);
	soa_alloc__particle_data_soa_t(&DomainPartBuf_soa, N);
}
void soa_free(void) {
	soa_free__sph_particle_data_soa_t(&SphP_soa);
	soa_free__particle_data_soa_t(&P_soa);
	soa_free__particle_data_soa_t(&DomainPartBuf_soa);
}

void soa_init__sph_particle_data_soa_t(struct sph_particle_data_soa_t *P) {
#ifdef HAVE__sph_particle_data_soa_t__d //;
	P->d = NULL;
#endif
}
void soa_alloc__sph_particle_data_soa_t(struct sph_particle_data_soa_t *P,
					size_t N) {
#ifdef HAVE__sph_particle_data_soa_t__d //;
	P->d = (double *)mymalloc("d", sizeof(*(P->d)) * N);
	if (!P->d)
		soa_abort(/*"allocating "*/"d");
#endif

}
void soa_free__sph_particle_data_soa_t(struct sph_particle_data_soa_t *P) {
#ifdef HAVE__sph_particle_data_soa_t__d //;
	P->d = NULL;
	if (P->d) {
		myfree(P->d);
		P->d = NULL;
	}
#endif
}

void soa_init__particle_data_soa_t(struct particle_data_soa_t *P) {
#ifdef HAVE__particle_data_soa_t__d //;
	P->d = NULL;
#endif
}
void soa_alloc__particle_data_soa_t(struct particle_data_soa_t *P, size_t N) {
#ifdef HAVE__particle_data_soa_t__d //;
	P->d = (double *)mymalloc("d", sizeof(*(P->d)) * N);
	if (!P->d)
		soa_abort(/*"allocating "*/"d");
#endif

}
void soa_free__particle_data_soa_t(struct particle_data_soa_t *P) {
#ifdef HAVE__particle_data_soa_t__d //;
	P->d = NULL;
	if (P->d) {
		myfree(P->d);
		P->d = NULL;
	}
#endif
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

	return 0;
}
