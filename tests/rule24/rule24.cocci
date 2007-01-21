@@
struct block_device *X;
@@

(
- block_size(to_kdev_t(X->bd_dev))
+ block_size(X)
|
- set_blocksize(to_kdev_t(X->bd_dev))
+ set_blocksize(X)
|
- blk_get_ra_pages(to_kdev_t(X->bd_dev))
+ blk_get_ra_pages(X)
)

@@
identifier x;
struct block_device *X;
@@

  x = to_kdev_t(X->bd_dev);
  ...
(
- block_size(x)
+ block_size(X)
|
- set_blocksize(x)
+ set_blocksize(X)
|
- blk_get_ra_pages(x)
+ blk_get_ra_pages(X)
)

// The following is not right, and I don't know how to express it
//@@
//kdev_t X;
//@@
//
//(
//- block_size(X)
//+ block_size(bdget(kdev_t_to_nr(X)))
//|
//- set_blocksize(X)
//+ set_blocksize(bdget(kdev_t_to_nr(X)))
//|
//- blk_get_ra_pages(X)
//+ blk_get_ra_pages(bdget(kdev_t_to_nr(X)))
//)