
typedef double MyFloat;

struct particle_data { MyFloat i; MyFloat j; } *P;

struct particle_data_soa_t { MyFloat*i; MyFloat*j; };


struct particle_data_soa_t P_soa;

int main ()
{
	P_soa.i[0]++;
}
