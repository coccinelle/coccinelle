
typedef double MyFloat;
typedef long double MyLongDouble;
typedef long double MyDoublePos;

typedef MyLongDouble MyLongDouble3[3];
typedef MyFloat MyFloat3[3];
enum cpufields {
  ENUM_SOMETHING
}; // ok for OpenGadget3, IPCC-Gadget

struct particle_data { 
 //MyFloat I[3][3];
 //MyFloat I[3][3];
 //MyFloat i[3];
 MyFloat3 j;
 //MyFloat J[3][3];
 //MyFloat i[3];
 //MyDoublePos K[3][3];
 MyLongDouble3 d;
 MyDoublePos k[3];
};

struct sph_particle_data { 
 //double I[3][3];
 double i[3];
 //double J[3][3];
 double j[3];
};

int main()
{
  return 0;
}
