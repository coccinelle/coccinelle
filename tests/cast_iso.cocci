@@
expression B;
type T;
@@

- snd_magic_cast(T, (void*) B ,...)
+ (T *)B

@@
expression B;
type T;
@@

- snd_magic_cast(T,B,...)
+ B
