// @@
// identifier x;
// // struct xx struc;
// @@
// (
// - sprintf
// |
// - strlcpy
// |
// - strcpy
// |
// - strcat
// |
// - snprintf
// )
// - (x.devfs_name, ...);


// the iso { E; } => E;   does not apply here :(

@@
identifier i;
@@
- for (i = ...; i < ...; i++) 
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
-}
