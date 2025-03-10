int main () {
  f(a);
  g(a,b);
  h(x,a,y,b,z);
  h(a,y,b,z);
  h(x,a,b,z);
  h(x,a,y,b);
  h(a,b,z);
  h(x,a,b);
  h(a,b);
}

int f(int a) { return; }

int g(int a,int b) { return; }

int h(int x, int a, int y, int b, int z) { return; }

int h(int a, int y, int b, int z) { return; }

int h(int x, int a, int b, int z) { return; }

int h(int x, int a, int y, int b) { return; }

int h(int a, int b, int z) { return; }

int h(int x, int a, int b) { return; }

int h(int a, int b) { return; }

int f[] = {
  a
 };

int g[] = {
  a,
  b
 };

int h[] = { x, a, y, b, z };

int h[] = { a, y, b, z, };

int h[] = { x, a, b, z };

int h[] = { x, a, y, b, };

int h[] = { a, b, z };

int h[] = { x, a, b, };

int h[] = { a, b };

int i[] = { x, a, y, b, z };

int i[] = { a, y, b, z, };

int i[] = { x, a, b, z };

int i[] = { x, a, y, b, };

int i[] = { a, b, z };

int i[] = { x, a, b, };

int i[] = { a, b };

struct f {
 int a;
};

struct g {
  int a;
  int b;
};

struct h {
  int x;
  int a;
  int y;
  int b;
  int z;
 };

struct h {
  int a;
  int y;
  int b;
  int z;
 };

struct h {
  int x;
  int a;
  int b;
  int z;
 };

struct h {
  int x;
  int a;
  int y;
  int b;
 };

struct h {
  int a;
  int b;
  int z;
 };

struct h {
  int x;
  int a;
  int b;
 };

struct h {
  int a;
  int b;
 };

enum f { a };

enum g { a, b };

enum h { x, a, y, b, z, };
enum h { a, y, b, z };
enum h { x, a, b, z, };
enum h { x, a, y, b };
enum h { a, b, z, };
enum h { x, a, b };
enum h { a, b, };

enum i { x, a, y, b, z, };
enum i { a, y, b, z };
enum i { x, a, b, z, };
enum i { x, a, y, b, };
enum i { a, b, z, };
enum i { x, a, b, };
enum i { a, b, };

#define f(a) 3

#define g(a,b) 3

#define h(x,a,y,b,z) 3
#define h(a,y,b,z) 3
#define h(x,a,b,z) 3
#define h(x,a,y,b) 3
#define h(a,b,z) 3
#define h(x,a,b) 3
#define h(a,b) 3
