int main () {
  f();
  g(b);
  h(x,y,b,z);
  h(y,b,z);
  h(x,b,z);
  h(x,y,b);
  h(b,z);
  h(x,b);
  h(b);
}

int f() { return; }

int g(int b) { return; }

int h(int x, int y, int b, int z) { return; }

int h(int y, int b, int z) { return; }

int h(int x, int b, int z) { return; }

int h(int x, int y, int b) { return; }

int h(int b, int z) { return; }

int h(int x, int b) { return; }

int h(int b) { return; }

int f[] = {
 };

int g[] = {
  b
 };

int h[] = { x, y, b, z };

int h[] = { y, b, z, };

int h[] = { x, b, z };

int h[] = { x, y, b, };

int h[] = { b, z };

int h[] = { x, b, };

int h[] = { b };

int i[] = { x, a, y, b, z };

int i[] = { a, y, b, z, };

int i[] = { x, a, b, z };

int i[] = { x, a, y, };

int i[] = { a, b, z };

int i[] = { x, a, };

int i[] = { a, };

struct f {
 int b;
};

struct g {
  int b;
};

struct h {
  int x;
  int y;
  int b;
  int z;
 };

struct h {
  int y;
  int b;
  int z;
 };

struct h {
  int x;
  int b;
  int z;
 };

struct h {
  int x;
  int y;
  int b;
 };

struct h {
  int b;
  int z;
 };

struct h {
  int x;
  int b;
 };

struct h {
  int b;
 };

enum f { b };

enum g { b };

enum h { x, y, b, z, };
enum h { y, b, z };
enum h { x, b, z, };
enum h { x, y, b };
enum h { b, z, };
enum h { x, b };
enum h { b, };

enum i { x, a, y, b, z, };
enum i { a, y, b, z };
enum i { x, a, b, z, };
enum i { x, a, y, };
enum i { a, b, z, };
enum i { x, a, };
enum i { a, };
