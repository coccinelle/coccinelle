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

//---------------------------------------------------------------------------
// extra CE
//---------------------------------------------------------------------------

@@ expression X; @@
- _snd_kcontrol_chip(X)
+ snd_kcontrol_chip(X)

@@ expression X; @@
- _snd_pcm_substream_chip(X)
+ snd_pcm_substream_chip(X)