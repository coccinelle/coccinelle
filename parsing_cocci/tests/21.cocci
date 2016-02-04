@@
expression A, B, C;
@@

- snd_magic_cast(A, B, C)
+ B

@@
expression X;
@@

- snd_magic_kfree(X)
+ kfree(X)

@@
expression X, A, C;
@@

- X = snd_magic_kcalloc(A, 0, C)
+ X = kcalloc(1, sizeof(*X), C)

@@
expression X, A, B, C;
@@

- X = snd_magic_kcalloc(A, B, C)
+ X = kcalloc(1, sizeof(*X)+B, C)

@@
expression X, A, C;
@@

- X = snd_magic_kmalloc(A, 0, C)
+ X = kmalloc(sizeof(*X), C)

@@
expression X, A, B, C;
@@

- X = snd_magic_kmalloc(A, B, C)
+ X = kmalloc(sizeof(*X)+B, C)
