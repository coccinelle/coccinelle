@@
identifier x;
identifier bdev;
@@
  struct block_device *bdev;
  <...
(
- block_size(x)
+ block_size(bdev)
|
- set_blocksize(x,
+ set_blocksize(bdev, 
  ...)
)
  ...>

@@
identifier x;
identifier fn;
identifier bdev;
@@
 fn(..., struct block_device *bdev, ...) {
 <...
(
- block_size(x)
+ block_size(bdev)
|
- set_blocksize(x,
+ set_blocksize(bdev, 
  ...)
)
 ...>
}

@@
identifier driver, minor;
@@

-	set_blocksize(mk_kdev(drive->channel->major, minor), CD_FRAMESIZE);

@@
kdev_t dev;
fresh identifier bdev;
statement S;
identifier bsize;
identifier fn;
@@
 fn(...) {
  ...
+	struct block_device *bdev = bdget(kdev_t_to_nr(dev));
	unsigned bsize = 
-         block_size(dev);
+         block_size(bdev);
-       S
+       bdput(bdev);
+       S
  ...
}
