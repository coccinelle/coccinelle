
struct particle_data       { int i;/* also in SoA now */ int j;/* also in SoA now */ int k; } * P ;
struct particle_data_soa_t { int i; int j;        } * P_soa ;

struct sph_particle_data       { int i;/* also in SoA now */ int j;/* also in SoA now */ int k; } * SphP ;
struct sph_particle_data_soa_t { int i; int j;        } * SphP_soa ;

int main()
{
	return 0;
}
