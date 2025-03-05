
extern struct sph_particle_data_buggy
{
#ifdef VORONOI
  int a;
#endif

  union // presence of this anonymous union causes previous two #endif's to get lost!
  {
    int dDivVel;
  } v;
}
  *DomainSphBuf_;

 struct sph_particle_data_buggy_soa_t {
#ifdef VORONOI
 	int a;
#endif
 };
 struct sph_particle_data_buggy_soa_t DomainSphBuf__soa;
 struct dens_eval_arg_t { };

int main()
{
	return 0;
}
