
typedef double MyLongDouble;
typedef double MyLongDouble3 [3];
typedef double MyDoublePos;
typedef double MyDoublePos3 [3];

struct particle_data {
 char c;
 double Entropy;
 MyLongDouble3 mld3;
 MyDoublePos Pos3[3];
 MyDoublePos Pos;
} *P;
struct particle_data_soa_t {
	double *Entropy;
	MyLongDouble3 *mld3;
	MyDoublePos *Pos;
};
extern struct particle_data_soa_t P_soa;
extern int maxThreads; // OpenGadget3, IPCC

int main()
{
	return 0;
}
