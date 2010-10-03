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

- init_MUTEX
+ mutex_init
  (&data.
-        lock
+        new_lock
  )

@@
type rule1.T;
identifier rule1.lock;
@@

T { ...
-   struct semaphore lock;
+   struct mutex new_lock;
    ...
  };
