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
identifier rule1.lock;
T *x;
@@

- x->lock
+ x->new_lock
