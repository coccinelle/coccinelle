
typedef double MyFloat;

typedef double double3x3[3][3];
typedef MyFloat MyFloat3x3[3][3];
enum cpufields {
  ENUM_SOMETHING
}; // ok for OpenGadget3, IPCC-Gadget

struct particle_data { 
 MyFloat3x3 I;
 MyFloat i[3];
 MyFloat3x3 J;
 MyFloat j[3];
};

struct sph_particle_data { 
 double3x3 I;
 double i[3];
 double3x3 J;
 double j[3];
};

int main()
{
  return 0;
}
