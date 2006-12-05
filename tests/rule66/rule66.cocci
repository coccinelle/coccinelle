// for a few files, this extra rule is needed
@@
expression B;
type T;
@@

- snd_magic_cast(T, (void*) B ,...)
+ (T *) B



@@
expression B;
type T;
@@

- snd_magic_cast(T,B,...)
+ B

@@
expression X;
@@

- snd_magic_kfree(X)
+ kfree(X)

@@
expression X, C;
type T;
@@

- X = snd_magic_kcalloc(T, 0, C)
+ X = kcalloc(1, sizeof(*X), C)


// if don't have the isomorphism-by-absence for cast
@@
expression X, C;
type T;
@@

- X = (T *) snd_magic_kcalloc(T, 0, C)
+ X = kcalloc(1, sizeof(*X), C)


// if don't have isomorphism affectation/initialisation
// but does not work yey
// @@
// expression C;
// identifier X;
// type T;
// @@
// 
// - T* X = (T *) snd_magic_kcalloc(T, 0, C);
// + T* X = kcalloc(1, sizeof(*X), C);



@@
expression X, B, C;
type T;
@@

- X = snd_magic_kcalloc(T, B, C)
+ X = kcalloc(1, sizeof(*X)+B, C)

@@
expression X, C;
type T;
@@

- X = snd_magic_kmalloc(T, 0, C)
+ X = kmalloc(sizeof(*X), C)

@@
expression X, B, C;
type T;
@@

- X = snd_magic_kmalloc(T, B, C)
+ X = kmalloc(sizeof(*X)+B, C)


// @@ text x;@@
// - #define chip_t x
// 
// //but julia will not detect that chip_t is a type
// //@@  @@
// //- chip_t
// //+ x
// @@ expression X; identifier var; @@
// - chip_t * var = X;
// + x * var = X;


//---------------------------------------------------------------------------
// extra CE
//---------------------------------------------------------------------------

@@ expression X; @@
- _snd_kcontrol_chip(X)
+ snd_kcontrol_chip(X)

@@ expression X; @@
- _snd_pcm_substream_chip(X)
+ snd_pcm_substream_chip(X)

@@ expression X; @@
- _snd_pcm_chip(X->pcm)
+ snd_pcm_substream_chip(X)