int f(double,double);
int g() {
  int i,j;
  int f();
  f(i,j);
  f(i);
}


/* peut faire plus simple */
struct t1 { int i,j;};
typedef int (*VF)(struct t1);
typedef int (*VF2)();
int g() {
  /* work for t1 and even when renamed in t2 */
  struct t2 { int i;};
  struct t2 v1, v2;
  VF f(); /* if this is put outside this namespace, then gcc dont complain */
  VF2 f(struct t2, struct t2);
  return (*(f(v1,v2)))(v2); /* gcc say nothing !! ?? */
}

/* TODO bug avec extern static, normally previous sur rien ca => external */

void main()
{
  /*  typedef struct t3 v;
  struct t3 { int i,j,k;};
  v *j;
  j->k = 3;
  struct t3 { int i,j;}; 
  */
}

