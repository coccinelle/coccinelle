@@
struct {input_dev, gameport} dev;
@@

(
- dev.idbus
+ dev.id.bustype
|
- dev.idvendor
+ dev.id.vendor
|
- dev.idproduct
+ dev.id.product
|
- dev.idversion
+ dev.id.version
)
