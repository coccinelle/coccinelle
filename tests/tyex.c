 typedef struct {
  double x;
  double y;
  char *name;
} Location;

int main () {
  Location a;
  Location *b;
  foo (a.x,a.y,a.name);
  foo (b->x,b->y,b->name);
}
