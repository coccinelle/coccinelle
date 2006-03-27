@@
expression X, Y;
@@

- bio_endio(X,Y)
(
+ bio_endio(X, bio_sectors(X) << 9, Y ? 0 : -EIO)
|
+ bio_endio(X, X->bi_size << 9, Y ? 0 : -EIO)
)

@@
expression X;
@@

- bio_io_error(X)
+ bio_io_error(X,X->bi_size)
