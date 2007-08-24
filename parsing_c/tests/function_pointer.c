

static int (*pointer_func)(int);

int main(int i) { 
  int (*f)(int i);

  f(0);
}


/*
int *(vtab[])
int * ((vtab)[])

int *(*vtab())

int * (*toto)(int c);


(* demo)(...)
tab[]


Pointer (Pointer)
basic  pointer direct_d
 []
 ()
 ident
 (recursive!)
 (pointer
 (direct_d)


*/
