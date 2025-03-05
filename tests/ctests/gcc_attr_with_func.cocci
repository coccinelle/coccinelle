@@
identifier f,a;
@@

int __attribute__((always_inline)) f
- (int a)
+ ()
{...}

@@
identifier f,a;
@@

int __attribute__((always_inline)) f
- (int a)
+ ()
;

@@
identifier f,a;
@@

int __attribute__((always_inline)) *f
- (int a)
+ (long a)
{...}

@@
identifier f,a;
@@

int f
- (int a)
+ (long a)
  __attribute__((attr))
{...}

@@
identifier f,a;
@@

 void __attribute__((mult,"1",2)) f
- (int a)
+ (long a)
 {...}

@@
identifier f,a;
@@

 void __attribute__((...,1,...)) f
- (int a)
+ (long a)
 {...}
