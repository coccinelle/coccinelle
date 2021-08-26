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
