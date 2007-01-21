@@
{ struct input_dev, struct gameport } E;
@@

(
- E.idbus
+ E.id.bus
|
- E.idvendor
+ E.id.vendor
|
- E.idproduct
+ E.id.product
|
- E.idversion
+ E.id.version
)
