@ rule1 @
type T;
identifier lock;
@@

T { ...
    struct semaphore lock;
    ...
  };

@@
type rule1.T;
T *x;
@@

- x->lock
+ x->new_lock
