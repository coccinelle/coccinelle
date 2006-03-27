struct t { int i; int j;};
enum t;
union v;

int main ()
{
  printf("%d\n", sizeof (struct t));
}
