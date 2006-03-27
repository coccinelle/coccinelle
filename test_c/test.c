typedef int small;

int f() {
  /*  enum { small }; */
  struct titi { enum { small } t; struct toto { int j;} v;};
  int j = sizeof(struct toto);
  small++;
}

unsigned short int i;
unsigned int short i;
short unsigned int i;
int unsigned short i;
short int unsigned i;										 
int short unsigned i;										 

int f(small j, int small);
/*int f(int small, small j);*/

int f(short small) 
{
  small++;
}
int f(int i) {
  /*  small++; */
}
int f() {
  int small[sizeof(small)];
  int small[] = { sizeof(small) } , j = sizeof(small);
}
/*

int g(toto,titi,small);
int f() {
  int small;
  int j = sizeof(small);
}

typedef int small;


int f(small small);

int f() {
 small[4];
small(int i);
}

int *f(int);
int (*v)(int);

*/
struct small { int i;};


small i;

int f(short small);

void main()
{
  int small;
  small++;
}
