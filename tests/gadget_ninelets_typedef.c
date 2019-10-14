
typedef double MyFloat;

enum cpufields {
  ENUM_SOMETHING
}; // ok for OpenGadget3, IPCC-Gadget

struct particle_data { 
 MyFloat I[3][3];
 MyFloat i[3];
 MyFloat J[3][3];
 MyFloat j[3];
};

struct sph_particle_data { 
 double I[3][3];
 double i[3];
 double J[3][3];
 double j[3];
};

int main()
{
  return 0;
}
