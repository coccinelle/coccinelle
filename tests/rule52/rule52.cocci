@@
{ struct input_dev, struct gameport } * E;
@@

(
- E->dev.idbus
+ E->dev.id.bus
|
- E->dev.idvendor
+ E->dev.id.vendor
|
- E->dev.idproduct
+ E->dev.id.product
|
- E->dev.idversion
+ E->dev.id.version
)
