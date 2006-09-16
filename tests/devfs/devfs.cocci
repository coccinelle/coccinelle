@@
identifier err;
statement S;
@@
- err = 
(
- devfs_mk_cdev
|
- devfs_mk_bdev
|
- devfs_mkdir
|
- devfs_mk_dir
|
- devfs_mk_symlink
|
- devfs_remove
)
- (...);
- if(err) S 


// cosmetic transformation handling. remove the { } when 
// goes from double to single instruction in the for.
@@
identifier i;
//statement S; // opti ?
identifier fn;
@@
 for (i = ...; i < ...; i++) 
-{
  fn(...);
(
- devfs_mk_cdev
|
- devfs_mk_bdev
|
- devfs_mkdir
|
- devfs_mk_dir
|
- devfs_mk_symlink
|
- devfs_remove
)
- (...);
-}


// cosmetic transformation handling.
// could facto if have ooo !
@@
identifier i;
//statement S; // opti ?
identifier fn;
@@
 for (i = ...; i < ...; i++) 
-{
(
- devfs_mk_cdev
|
- devfs_mk_bdev
|
- devfs_mkdir
|
- devfs_mk_dir
|
- devfs_mk_symlink
|
- devfs_remove
)
- (...);
  fn(...);
//  S
-}



@@
identifier i;
@@
- for (i = ...; i < ...; i++) 
(
- { devfs_mk_cdev(...); }
|
- { devfs_mk_bdev(...); }
|
- { devfs_mkdir(...); }
|
- { devfs_mk_dir(...); }
|
- { devfs_mk_symlink(...); }
|
- { devfs_remove(...); }
)



//----------------------------------------------------------------------------
@@
@@
(
- devfs_mk_cdev
|
- devfs_mk_bdev
|
- devfs_mkdir
|
- devfs_mk_dir
|
- devfs_mk_symlink
|
- devfs_remove
)
- (...);



//----------------------------------------------------------------------------

@@ @@
- TTY_DRIVER_NO_DEVFS
+ TTY_DRIVER_DYNAMIC_DEV

//----------------------------------------------------------------------------
// @@ @@
// 
// -#include <linux/devfs_fs_kernel.h>


//----------------------------------------------------------------------------
@@
identifier x;
// struct xx struc;
@@
(
- sprintf
|
- strlcpy
|
- strcpy
|
- strcat
|
- snprintf
)
- (x->devfs_name, ...);

@@
expression E;
expression E2;
@@

//- E->devfs_name = ...;
(
- E->devfs_name = E2;
|
- E->devfs_name[...] = E2;
)

//----------------------------------------------------------------------------
@@
expression X;
@@
- X = devfs_register_tape(...); 

@@
@@
- devfs_unregister_tape(X);
