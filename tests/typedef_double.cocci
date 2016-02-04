@ rule1 @
type T;
identifier lock;
@@

T { ...
    struct semaphore lock;
    ...
  };

@ rule1a @
type rule1.T;
T data;
identifier rule1.lock;
@@

- sema_init
+ mutex_init
  (&data.lock)
