 typedef struct {
  double x;
  double y;
} Point;
typedef struct {
  char *name;
  Point p;
} Location;

int main () {
  Location a;
  Location *b;
  foo (a.p.x,a.p.y,a.name);
  foo (b->p.x,b->p.y,b->name);
}
