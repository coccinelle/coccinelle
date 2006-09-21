// @@
// identifier i;
// identifier func;
// @@
// 
//  func(...) { 
//   ...
// -  int i;
//   <... when != i
// - for (i = ...; i < ...; i++) 
// (
// - devfs_mk_cdev
// |
// - devfs_mk_bdev
// |
// - devfs_mkdir
// |
// - devfs_mk_dir
// |
// - devfs_mk_symlink
// |
// - devfs_remove
// )
// - (...);
//   ...>
//  }
// 
// 
// @@
// identifier i;
// identifier func;
// @@
// 
//  func(...) { 
//   ...
//   int i;
//   <... 
// - for (i = ...; i < ...; i++) 
// (
// - devfs_mk_cdev
// |
// - devfs_mk_bdev
// |
// - devfs_mkdir
// |
// - devfs_mk_dir
// |
// - devfs_mk_symlink
// |
// - devfs_remove
// )
// - (...);
//   ...> 
//  }


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
 // { ... } does not work, so have to use S

// remove also the int err; ?


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

// Does not work with isomorphisms. Isopattern can not be applied here
// cos I guess all the token are not minus


//expression A;
// A 
//- | TTY_DRIVER_NO_DEVFS
//+ | TTY_DRIVER_DYNAMIC_DEV

// Here the iso can be applied.
// but when SP match in inverted, we dont generate the inverted
// because we still generate A | TTY_DRIVER_DYNAMIC_DEV whereas
// we would like TTY_DRIVER_DYNAMIC_DEV | A if it was match like that.
// but not a big pb.
//- A | TTY_DRIVER_NO_DEVFS
//+ A | TTY_DRIVER_DYNAMIC_DEV

//----------------------------------------------------------------------------
// @@ @@
// 
// -#include <linux/devfs_fs_kernel.h>


//----------------------------------------------------------------------------
@@
identifier x;
// struct xx struc;
identifier fn;
@@
//(
//- sprintf
//|
//- strlcpy
//|
//- strcpy
//|
//- strcat
//|
//- snprintf
//)
- fn(x->devfs_name, ...);

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


// todo: how handle struct field initializer ?
// 	.devfs_name             = ...,

// todo: must also remove the unusedfield from the
// struct def

//----------------------------------------------------------------------------
@@
expression X;
//struct xx struc;
//identifier unusedfield;
@@
- X = devfs_register_tape(...); 
// struc->unusedfield=devfs_register_tape(d->devfs_name);

@@ @@
- devfs_unregister_tape(X);
// devfs_unregister_tape(struc->unusedfield);

// error words [devfs]
